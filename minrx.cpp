//
// MinRX: a minimal matcher for POSIX Extended Regular Expressions.
// Copyright (C) 2023, 2024, 2025 Michael J. Haertel.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS “AS IS” AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.
//

#include <cctype>
#include <climits>
#include <clocale>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <cwchar>
#include <cwctype>
#include <algorithm>
#include <deque>
#include <initializer_list>
#include <limits>
#include <map>
#include <mutex>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <vector>
#ifdef CHARSET
#include <memory>
#include "charset.h"
#endif
#include "minrx.h"

#ifdef __GNUC__
#define INLINE __attribute__((__always_inline__)) inline
#else
#define INLINE inline
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_GETTEXT_H
#include <gettext.h>
#define _(msgid)  gettext(msgid)
#else /* ! HAVE_GETTEXT_H */
#define _(msgid)  msgid
#endif /* ! HAVE_GETTEXT_H */

#define N_(msgid) msgid

namespace MinRX {

template <typename UINT> auto ctz(UINT x) { return __builtin_ctz(x); }
template <> auto ctz(unsigned long x) { return __builtin_ctzl(x); }
template <> auto ctz(unsigned long long x) { return __builtin_ctzll(x); }

template <typename TYPE, TYPE INIT = 0>
struct COWVec {
	struct Storage;
	struct Allocator {
		std::size_t length;
		Storage *freelist = nullptr;
		Allocator(std::size_t length)
		: length(length)
		{}
		~Allocator() {
			for (Storage *s = freelist, *sfreelink = nullptr; s != nullptr; s = sfreelink) {
				sfreelink = s->u.freelink;
				::operator delete(s);
			}
		}
		Storage *alloc() {
			Storage *r;
			if (freelist) {
				r = freelist;
				freelist = r->u.freelink;
				r->u.allocator = this;
				r->refcnt = 1;
			} else {
				void *p = ::operator new(sizeof (Storage) + (length - 1) * sizeof (TYPE));
				r = new (p) Storage(*this);
			}
			for (std::size_t i = 0; i != length; ++i)
				(*r)[i] = INIT;
			return r;
		}
		void dealloc(Storage *s) {
			s->u.freelink = freelist;
			freelist = s;
		}
	};
	struct Storage {
		union {
			Allocator *allocator;
			Storage *freelink;
		} u;
		std::size_t refcnt;
		TYPE hack[1];
		const TYPE &operator[](std::size_t i) const { return (&hack[0])[i]; }
		TYPE &operator[](std::size_t i) { return (&hack[0])[i]; }
		Storage *clone() {
			auto s = u.allocator->alloc();
			for (std::size_t i = 0; i != u.allocator->length; ++i)
				(*s)[i] = (*this)[i];
			return s;
		}
		Storage(Allocator &a)
		: u({&a})
		, refcnt(1)
		{}
	};
	Storage *storage;
	COWVec(): storage(nullptr) {}
	COWVec(Allocator &a): storage(a.alloc()) {}
	COWVec(const COWVec &cv): storage(cv.storage) { ++storage->refcnt; }
	COWVec(COWVec &&cv): storage(cv.storage) { cv.storage = nullptr; }
	COWVec &operator=(const COWVec &cv) {
		++cv.storage->refcnt;
		if (storage && --storage->refcnt == 0)
			storage->u.allocator->dealloc(storage);
		storage = cv.storage;
		return *this;
	}
	COWVec &operator=(COWVec &&cv) {
		if (storage && --storage->refcnt == 0)
			storage->u.allocator->dealloc(storage);
		storage = cv.storage;
		cv.storage = nullptr;
		return *this;
	}
	~COWVec() { if (storage && --storage->refcnt == 0) storage->u.allocator->dealloc(storage); }
	template <typename... XArgs>
	bool cmpgt(const COWVec &other, std::size_t limit, XArgs... xargs) const {
		std::size_t i;
		TYPE *xv = &(*storage)[0];
		TYPE *yv = &(*other.storage)[0];
		for (i = 0; i < limit - sizeof... (XArgs); i++)
			if (xv[i] != yv[i])
				return xv[i] > yv[i];
		if constexpr (sizeof...(XArgs) > 0)
			for (TYPE x : { xargs... })
				if (x != yv[i++])
					return x > yv[i - 1];
		return false;
	}
	const TYPE &get(std::size_t idx) const { return (*storage)[idx]; }
	COWVec &put(std::size_t idx, TYPE val) {
		if ((*storage)[idx] == val)
			return *this;
		if (storage->refcnt > 1) {
			--storage->refcnt;
			storage = storage->clone();
			storage->refcnt = 1;
		}
		(*storage)[idx] = val;
		return *this;
	}
};

template <typename UINT>
struct QSet {
	std::uint64_t *bits[10];
	int depth = 0;
	QSet(UINT limit) {
		std::size_t s[10], t = 0;
		do
			t += (limit = s[depth++] = (limit + 63u) / 64u);
		while (limit > 1);
		std::uint64_t *next = (std::uint64_t *) ::operator new(t * sizeof (std::uint64_t));
		for (int i = 0; i < depth; ++i)
			bits[i] = next, next += s[depth - 1 - i];
		bits[0][0] = 0;
	}
	~QSet() { ::operator delete(bits[0]); }
	static std::uint64_t bit(UINT k) { return (std::uint64_t) 1 << (k & 0x3F); }
	bool empty() const { return !bits[0][0]; }
	bool contains(UINT k) const {
		int i = 0, s = 6 * depth;
		UINT j = 0;
		while (i < depth) {
			auto x = bits[i++][j];
			s -= 6;
			j = k >> s;
			auto w = bit(j);
			if (!(x & w))
				return false;
		}
		return true;
	}
	bool insert(UINT k) {
		bool r = false;
		int i = 0, s = 6 * depth;
		UINT j = 0;
		while (i < depth) {
			auto bp = &bits[i++][j];
			auto x = *bp;
			s -= 6;
			j = k >> s;
			auto w = bit(j);
			if ((x & w) == 0) {
				if (i < depth)
					bits[i][j] = 0;
				else
					r = true;
			}
			*bp = x | w;
		}
		return r;
	}
	UINT remove() { // caller must ensure !empty()
		UINT k = 0;
		int i = 0, d = depth;
		do
			k = (k << 6) | ctz(bits[i++][k]);
		while (i != d);
		UINT r = k;
		do {
			--i;
			auto w = bit(k);
			k >>= 6;
			if ((bits[i][k] &= ~w) != 0)
				break;
		} while (i != 0);
		return r;
	}
};

template <typename UINT, typename DATA>
struct QVec {
	QSet<UINT> qset;
	DATA *storage;
	QVec(UINT l): qset(l), storage(static_cast<DATA *>(::operator new(l * sizeof (DATA)))) {}
	~QVec() {
		clear();
		::operator delete(storage);
	}
	void clear() {
		while (!qset.empty()) {
			auto i = qset.remove();
			storage[i].~DATA();
		}
	}
	bool contains(UINT k) const { return qset.contains(k); }
	bool empty() const { return qset.empty(); }
	std::tuple<bool, DATA&> insert(UINT k, const DATA& v) {
		bool newly = qset.insert(k);
		// WARNING: if newly inserted then we are returning a reference to uninitialized memory
		// and it is the caller's responsibility to construct valid DATA there.
		return {newly, storage[k]};
	}
	DATA &lookup(UINT k) { return storage[k]; }
	const DATA &lookup(UINT k) const { return storage[k]; }
	std::tuple<UINT, DATA> remove() {
		auto k = qset.remove();
		return {k, std::move(storage[k])};
	}
};

typedef int32_t WChar;			// because wchar_t may not be 32 bits
constexpr int32_t WCharMax = 0x10FFFF;	// maximum code point: valid for Unicode and (FIXME!) blithely assumed for non-Unicode
class WConv {
public:
	enum { End = -1 };
	enum class Encoding { Byte, MBtoWC, UTF8 };
private:
	WConv &(WConv::*const nextfn)();
	const char *const bp;
	const char *const ep;
	const char *cp;
	std::mbstate_t mbs;
	WChar wch = End;
	int len = 0;
	static WConv &(WConv::*const nextfns[])();
public:
	WConv(const WConv &) = default;
	WConv(Encoding e, const char *bp, const char *ep)
	: nextfn(nextfns[(int) e]), bp(bp), ep(ep), cp(bp) {
		std::memset(&mbs, 0, sizeof mbs);
	}
	WConv(Encoding e, const char *bp): WConv(e, bp, bp + std::strlen(bp)) { }
	auto look() const { return wch; }
	auto lookahead() const { return WConv(*this).nextchr().look(); }
	WConv &nextchr() { return (this->*nextfn)(); }
	WConv &nextbyte() {
		if ((cp += len) != ep)
			len = 1, wch = (unsigned char) *cp;
		else
			len = 0, wch = End;
		return *this;
	}
	WConv &nextmbtowc() {
		wchar_t wct = L'\0';
		if ((cp += len) != ep) {
			auto n = mbrtowc(&wct, cp, ep - cp, &mbs);
			if (n == 0 || n == (std::size_t) -1 || n == (std::size_t) -2) {
				len = 1;
				if (wct == L'\0')
					wct = std::numeric_limits<WChar>::min() + (unsigned char) *cp;
			} else {
				len = n;
			}
			wch = wct;
		} else {
			len = 0, wch = End;
		}
		return *this;
	}
	WConv &nextutf8() {
		if ((cp += len) != ep) {
			WChar u = (unsigned char) cp[0];
			if (u < 0x80) {
				len = 1, wch = u;
				return *this;
			}
			if ((u & 0x40) == 0 || cp + 1 == ep) {
			error:
				len = 1, wch = std::numeric_limits<WChar>::min() + u;
				return *this;
			}
			WChar v = (unsigned char) cp[1];
			if ((v & 0xC0) != 0x80)
				goto error;
			if ((u & 0x20) == 0) {
				WChar r = ((u & 0x1F) << 6) | (v & 0x3F);
				if (r < 0x80)
					goto error;
				len = 2, wch = r;
				return *this;
			}
			if (cp + 2 == ep)
				goto error;
			WChar w = (unsigned char) cp[2];
			if ((w & 0xC0) != 0x80)
				goto error;
			if ((u & 0x10) == 0) {
				WChar r = ((u & 0x0F) << 12) | ((v & 0x3F) << 6) | (w & 0x3F);
				if (r < 0x800)
					goto error;
				len = 3, wch = r;
				return *this;
			}
			if (cp + 3 == ep)
				goto error;
			WChar x = (unsigned char) cp[3];
			if ((x & 0xC0) != 0x80)
				goto error;
			if ((u & 0x08) != 0)
				goto error;
			WChar r = ((u & 0x07) << 18) | ((v & 0x3F) << 12) | ((w & 0x3F) << 6) | (x & 0x3F);
			if (r < 0x010000 || r > 0x10FFFF)
				goto error;
			len = 4, wch = r;
			return *this;
		} else {
			len = 0, wch = End;
			return *this;
		}
	}
	std::size_t off() const { return cp - bp; }
	auto ptr() const { return cp; }
	auto save() { return std::make_tuple(cp, wch, len); }
	void restore(std::tuple<const char *, WChar, int> t) { std::tie(cp, wch, len) = t; }
};

WConv &(WConv::*const WConv::nextfns[3])() = { &WConv::nextbyte, &WConv::nextmbtowc, &WConv::nextutf8 };

struct CSet {
#ifdef CHARSET
	charset_t *charset = nullptr;
	CSet(WConv::Encoding enc) {
		int errcode = 0;
		charset = charset_create(& errcode, MB_CUR_MAX, enc == WConv::Encoding::UTF8);
		// FIXME: Throw error if charset == nullptr
	}
	CSet(const CSet &) = delete;
	CSet &operator=(const CSet &) = delete;
	CSet(CSet &&cs): charset(cs.charset) { cs.charset = nullptr; }
	CSet &operator=(CSet &&cs) { charset = cs.charset; cs.charset = nullptr; return *this; }
	CSet &operator|=(const CSet &cs) {
		charset_merge(charset, cs.charset);
		return *this;
	}
	~CSet() { if (charset) { charset_free(charset); charset = nullptr; } }
#else
	static std::map<std::string, CSet> cclmemo;
	static std::mutex cclmutex;
	struct Range {
		Range(WChar x, WChar y): min(std::min(x, y)), max(std::max(x, y)) {}
		WChar min, max;
		int operator<=>(const Range &r) const {
			return (min > r.max) - (max < r.min);
		}
	};
	std::set<Range> ranges;
	CSet(WConv::Encoding) { }
	CSet &operator|=(const CSet &cs) {
		for (const auto &e : cs.ranges)
			set(e.min, e.max);
		return *this;
	}
#endif
	CSet &invert() {
#ifdef CHARSET
		int errcode = 0;
		charset_t *newset = charset_invert(charset, &errcode); // FIXME: no error checking
		charset_free(charset);
		charset = newset;
#else
		std::set<Range> nranges;
		WChar lo = 0;
		for (const auto &e : ranges) {
			if (lo < e.min)
				nranges.emplace(lo, e.min - 1);
			lo = e.max + 1;
		}
		if (lo <= WCharMax)
			nranges.emplace(lo, WCharMax);
		ranges = std::move(nranges);
#endif
		return *this;
	}
	CSet &set(WChar wclo, WChar wchi) {
#ifdef CHARSET
		charset_add_range(charset, wclo, wchi);	// FIXME: no error checking
#else
		auto e = Range(wclo - (wclo != std::numeric_limits<WChar>::min()), wchi + (wchi != std::numeric_limits<WChar>::max()));
		auto [x, y] = ranges.equal_range(e);
		if (x == y) {
			ranges.insert(Range(wclo, wchi));
		} else {
			if (x->max >= e.min)
				wclo = std::min(wclo, x->min);
			auto z = y;
			--z;
			if (z->min <= e.max)
				wchi = std::max(wchi, z->max);
			auto i = ranges.erase(x, y);
			ranges.insert(i, Range(wclo, wchi));
		}
#endif
		return *this;
	}
	CSet &set(WChar wc) {
#ifdef CHARSET
		charset_add_char(charset, wc);	// FIXME: no error checking
		return *this;
#else
		return set(wc, wc);
#endif
	}
	bool test(WChar wc) const {
#ifdef CHARSET
		return charset_in_set(charset, wc);
#else
		if (wc < 0)
			return false;
		auto i = ranges.lower_bound(Range(wc, wc));
		return i != ranges.end() && wc >= i->min && wc <= i->max;
#endif
	}
	bool cclass(minrx_regcomp_flags_t flags, WConv::Encoding enc, const std::string &name) {
#ifdef CHARSET
		int result = charset_add_cclass(charset, name.c_str());
		if ((flags & MINRX_REG_ICASE) != 0) {
			if (name == "lower")
				charset_add_cclass(charset, "upper");	// FIXME: Add error checking
			else if (name == "upper")
				charset_add_cclass(charset, "lower");	// FIXME: Add error checking
		}
		return result == CSET_SUCCESS;
#else
		auto wct = std::wctype(name.c_str());
		if (wct) {
			std::string key = name + ":" + std::setlocale(LC_CTYPE, NULL) + ":" + ((flags & MINRX_REG_ICASE) != 0 ? "1" : "0");
			std::lock_guard<std::mutex> lock(cclmutex);
			auto i = cclmemo.find(key);
			if (i == cclmemo.end()) {
				if (enc == WConv::Encoding::Byte)
					for (WChar b = 0; b <= 0xFF; ++b) {
						if (std::iswctype(std::btowc(b), wct)) {
							set(b);
							if ((flags & MINRX_REG_ICASE) != 0) {
								set(std::tolower(b));
								set(std::toupper(b));
							}
						}
					}
				else
					for (WChar wc = 0; wc <= WCharMax; ++wc) {
						if (std::iswctype(wc, wct)) {
							set(wc);
							if ((flags & MINRX_REG_ICASE) != 0) {
								set(std::towlower(wc));
								set(std::towupper(wc));
							}
						}
					}
				cclmemo.emplace(key, *this);
				i = cclmemo.find(key);
			}
			*this |= i->second; // N.B. could probably be safely outside the critical section, since cclmemo entries are never deleted
			return true;
		}
		return false;
#endif
	}
#ifndef CHARSET
	void add_equiv(int32_t equiv) {
		wchar_t wcs_in[2];
		wchar_t wcs[2];
		wchar_t abuf[100], wbuf[100];

		wcs_in[0] = equiv;
		wcs_in[1] = 0;
		wcsxfrm(abuf, wcs_in, 99);
		wcs[1] = 0;
		for (wchar_t u = 1; u <= WCharMax; ++u) {
			wcs[0] = u;
			wcsxfrm(wbuf, wcs, 99);
			if (abuf[0] == wbuf[0])
				set(u);
		}
	}
#endif
	minrx_result_t parse(minrx_regcomp_flags_t flags, WConv::Encoding enc, WConv &wconv) {
		auto wc = wconv.nextchr().look();
		bool inv = wc == L'^';
		if (inv)
			wc = wconv.nextchr().look();
		for (bool first = true;; first = false) {
			auto wclo = wc, wchi = wc;
			if (wclo == WConv::End)
				return MINRX_REG_EBRACK;
			wc = wconv.nextchr().look();
			if (wclo == L']' && !first)
				break;
			if (wclo == L'\\' && (flags & MINRX_REG_BRACK_ESCAPE) != 0) {
				if (wc != WConv::End) {
					wclo = wchi = wc;
					wc = wconv.nextchr().look();
				} else {
					return MINRX_REG_EESCAPE;
				}
			} else if (wclo == L'[') {
				if (wc == L'.') {
					wc = wconv.nextchr().look();
					wclo = wchi = wc;
#ifdef CHARSET_NOT_YET
					int32_t coll[2] = { wc, L'\0' };
					charset_add_collate(charset, coll);	// FIXME: No error checking
					if ((flags & MINRX_REG_ICASE) != 0) {
						if (std::iswlower(wc))
							coll[0] = std::towupper(wc);
						else if (std::iswupper(wc))
							coll[0] = std::towlower(wc);
						charset_add_collate(charset, coll);	// FIXME: No error checking
					}
#endif
					wc = wconv.nextchr().look();
					if (wc != L'.' || (wc = wconv.nextchr().look() != L']'))
						return MINRX_REG_ECOLLATE;
				} else if (wc == L':') {
					wconv.nextchr();
					auto bp = wconv.ptr();
					while (wconv.look() != WConv::End && wconv.look() != L':')
						wconv.nextchr();
					if (wconv.look() != L':')
						return MINRX_REG_ECTYPE;
					auto ep = wconv.ptr();
					wconv.nextchr();
					if (wconv.look() != L']')
						return MINRX_REG_ECTYPE;
					wc = wconv.nextchr().look();
					auto cclname = std::string(bp, ep);
					if (cclass(flags, enc, cclname))
						continue;
					return MINRX_REG_ECTYPE;
				} else if (wc == L'=') {
					wc = wconv.nextchr().look();
					wclo = wchi = wc;
#ifdef CHARSET
					charset_add_equiv(charset, wc);	// FIXME: No error checking
					if ((flags & MINRX_REG_ICASE) != 0) {
						if (std::iswlower(wc))
							charset_add_equiv(charset, std::towupper(wc));	// FIXME: no error checking
						else if (std::iswupper(wc))
							charset_add_equiv(charset, std::towlower(wc));	// FIXME: no error checking
					}
#else
					add_equiv(wc);
					if ((flags & MINRX_REG_ICASE) != 0) {
						if (std::iswlower(wc))
							add_equiv(std::towupper(wc));
						else if (std::iswupper(wc))
							add_equiv(std::towlower(wc));
					}
#endif
					wc = wconv.nextchr().look();
					if (wc != L'=' || (wc = wconv.nextchr().look() != L']'))
						return MINRX_REG_ECOLLATE;
				}
			}
			bool range = false;
			if (wc == L'-') {
				auto save = wconv.save();
				wc = wconv.nextchr().look();
				if (wc == WConv::End)
					return MINRX_REG_EBRACK;
				if (wc != L']') {
					wchi = wc;
					wc = wconv.nextchr().look();
					if (wchi == L'\\' && (flags & MINRX_REG_BRACK_ESCAPE) != 0) {
						if (wc != WConv::End) {
							wchi = wc;
							wc = wconv.nextchr().look();
						} else {
							return MINRX_REG_EESCAPE;
						}
					} else if (wchi == L'[') {
						if (wc == L'.') {
							wchi = wconv.nextchr().look();
							wc = wconv.nextchr().look();
							if (wc != L'.' || (wc = wconv.nextchr().look()) != L']')
								return MINRX_REG_ECOLLATE;
							wc = wconv.nextchr().look();
						} else if (wc == L':' || wc == L'=') {
							return MINRX_REG_ERANGE; // can't be range endpoint
						}
					}
					range = true;
				} else {
					wconv.restore(save);
					wc = L'-';
				}
			}
			if (wclo > wchi || (wclo != wchi && (wclo < 0 || wchi < 0)))
				return MINRX_REG_ERANGE;
			if (wclo >= 0) {
				set(wclo, wchi);
				if ((flags & MINRX_REG_ICASE) != 0) {
					for (auto wc = wclo; wc <= wchi; ++wc) {
						set(enc == WConv::Encoding::Byte ? std::tolower(wc) : std::towlower(wc));
						set(enc == WConv::Encoding::Byte ? std::toupper(wc) : std::towupper(wc));
					}
				}
			}
			if (range && wc == L'-' && wconv.lookahead() != L']')
				return MINRX_REG_ERANGE;
		}
		if (inv) {
			if ((flags & MINRX_REG_NEWLINE) != 0)
				set(L'\n');
			invert();
		}
		return MINRX_REG_SUCCESS;
	}
};

#ifndef CHARSET
std::map<std::string, CSet> CSet::cclmemo;
std::mutex CSet::cclmutex;
#endif

typedef std::size_t NInt;

struct Node {
	enum Type {
		// character-matching node types
		/* char <= uchar max */	// no args
		CSet = WCharMax + 1,	// args = index in Regexp::csets vector
		// epsilon-matching node types
		Exit,			// no args
		Fork,			// args = offset to first goto
		Goto,			// args = offset to next goto, offset to just after join
		Join,			// args = none (but supplies incoming stack depth for next node)
		Loop,			// args = offset to next, optional flag
		Next,			// args = offset to loop, infinite flag
		Skip,			// args = offset over skipped nodes
		SubL,			// args = minimum and maximum contained subexpression numbers
		SubR,			// args = minimum and maximum contained subexpression numbers
		ZBOB,			// no args - match empty string at beginning of buffer
		ZEOB,			// no args - match empty string at end of buffer
		ZBOL,			// no args - match empty string at beginning of buffer or following \n
		ZEOL,			// no args - match empty string at end of buffer or preceding \n
		ZBOW,			// no args - match empty string at beginning of word
		ZEOW,			// no args - match empty string at end of word
		ZXOW,			// no args - match empty string at either end of word
		ZNWB			// no args - match empty string at non-word-boundary
	};
	NInt type;
	NInt args[2];
	NInt nstk;
};

struct Regexp {
	const std::vector<CSet> csets;
	const std::vector<Node> nodes;
	std::size_t nstk;
	std::size_t nsub;
	WConv::Encoding enc;
	minrx_result_t err;
};

struct Compile {
	const minrx_regcomp_flags_t flags;
	WConv::Encoding enc;
	WConv wconv;
	std::vector<CSet> csets;
	std::optional<std::size_t> dot;
	std::optional<std::size_t> esc_s;
	std::optional<std::size_t> esc_S;
	std::optional<std::size_t> esc_w;
	std::optional<std::size_t> esc_W;
	std::map<WChar, unsigned int> icmap;
	NInt nsub = 0;
	Compile(WConv::Encoding e, const char *bp, const char *ep, minrx_regcomp_flags_t flags): flags(flags), enc(e), wconv(e, bp, ep) { wconv.nextchr(); }
	bool num(NInt &n) {
		auto satmul = [](NInt x, NInt y) -> NInt {
			return (x == 0 || y == 0) ? 0 : ((x * y / x == y) ? x * y : -1);
		};
		auto wc = wconv.look();
		if (wc < L'0' || wc > L'9')
			return false;
		NInt v = 0;
		do {
			v = satmul(v, 10);
			if (v == (NInt) -1 || (v += wc - L'0') < (NInt) wc - L'0') {
				do
					wc = wconv.nextchr().look();
				while (wc >= L'0' && wc <= L'9');
				n = -1;
				return true;
			}
			wc = wconv.nextchr().look();
		} while (wc >= L'0' && wc <= L'9');
		n = v;
		return true;
	}
	typedef std::tuple<std::deque<Node>, std::size_t, minrx_result_t> Subexp;
	Subexp alt(bool nested, NInt nstk) {
		auto [lhs, lhmaxstk, err] = cat(nested, nstk);
		if (err)
			return {lhs, lhmaxstk, err};
		if (wconv.look() == L'|') {
			for (auto &l : lhs)
				l.nstk += 1;
			std::vector<Subexp> alts;
			while (wconv.look() == L'|') {
				wconv.nextchr();
				alts.push_back(cat(nested, nstk + 1));
			}
			auto [rhs, rhmaxstk, err] = alts.back();
			if (err)
				return {rhs, rhmaxstk, err};
			rhs.push_front({Node::Goto, {rhs.size(), rhs.size() + 1}, nstk + 1});
			alts.pop_back();
			while (!alts.empty()) {
				auto [mhs, mhmaxstk, _] = alts.back();
				alts.pop_back();
				rhs.insert(rhs.begin(), mhs.begin(), mhs.end());
				rhmaxstk = std::max(mhmaxstk, rhmaxstk);
				rhs.push_front({Node::Goto, {mhs.size(), rhs.size() + 1}, nstk + 1});
			}
			lhs.push_front({Node::Fork, { lhs.size(), 0 }, nstk + 1});
			lhs.insert(lhs.end(), rhs.begin(), rhs.end());
			lhmaxstk = std::max(lhmaxstk, rhmaxstk);
			lhs.push_back({Node::Join, {lhs.size() - 1, 0}, nstk + 1});
		}
		return {lhs, lhmaxstk, MINRX_REG_SUCCESS};
	}
	Subexp cat(bool nested, NInt nstk) {
		auto [lhs, lhmaxstk, err] = rep(nested, nstk);
		if (err)
			return {lhs, lhmaxstk, err};
		auto wc = wconv.look();
		while (wc != WConv::End && wc != L'|' && (wc != L')' || !nested)) {
			auto [rhs, rhmaxstk, err] = rep(nested, nstk);
			if (err)
				return {rhs, rhmaxstk, err};
			lhs.insert(lhs.end(), rhs.begin(), rhs.end());
			lhmaxstk = std::max(lhmaxstk, rhmaxstk);
			wc = wconv.look();
		}
		return {lhs, lhmaxstk, MINRX_REG_SUCCESS};
	}
	Subexp mkrep(const Subexp &lh, bool optional, bool infinite, NInt nstk) {
		auto [lhs, lhmaxstk, _] = lh;
		if (optional && !infinite) {
			for (auto &l : lhs) l.nstk += 1;
			auto lhsize = lhs.size();
			lhs.push_front({Node::Skip, {lhsize, 0}, nstk + 1});
			return {lhs, lhmaxstk + 1, MINRX_REG_SUCCESS};
		} else {
			for (auto &l : lhs) l.nstk += 3;
			auto lhsize = lhs.size();
			lhs.push_front({Node::Loop, {lhsize, (NInt) optional}, nstk + 3});
			lhs.push_back({Node::Next, {lhsize, (NInt) infinite}, nstk});
			return {lhs, lhmaxstk + 3, MINRX_REG_SUCCESS};
		}
	}
	Subexp mkrep(const Subexp &lh, NInt m, NInt n, NInt nstk) {
		if ((m != (NInt) -1 && m > RE_DUP_MAX) || (n != (NInt) -1 && n > RE_DUP_MAX) || m > n)
			return {{}, 0, MINRX_REG_BADBR};
		if (n == 0)
			return {{}, 0, MINRX_REG_SUCCESS};
		if (m == 0 && n == 1)
			return mkrep(lh, true, false, nstk);
		if (m == 0 && n == (NInt) -1)
			return mkrep(lh, true, true, nstk);
		if (m == 1 && n == 1)
			return lh;
		if (m == 1 && n == (NInt) -1)
			return mkrep(lh, false, true, nstk);
		auto [lhs, lhmaxstk, _] = lh;
		auto [rhs, rhmaxstk, __] = lh;
		NInt k;
		for (k = 1; k < m; ++k)
			lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		if (n != (NInt) -1 && k < n) {
			lhmaxstk += 1;
			rhmaxstk += 1;
			for (auto &r : rhs)
				r.nstk += 1;
			auto rhsize = rhs.size();
			rhs.push_front({Node::Skip, {rhsize, 0}, nstk + 1});
			for (; k < n; ++k)
				lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		}
		if (n == (NInt) -1) {
			lhmaxstk += 3;
			rhmaxstk += 3;
			for (auto &r : rhs)
				r.nstk += 3;
			auto rhsize = rhs.size();
			rhs.push_front({Node::Loop, {rhsize, 0}, nstk + 3});
			rhs.push_back({Node::Next, {rhsize, 1}, nstk});
			lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		}
		if (m == 0)
			return mkrep({lhs, rhmaxstk, MINRX_REG_SUCCESS}, true, false, nstk);
		else
			return {lhs, rhmaxstk, MINRX_REG_SUCCESS};
	}
	Subexp rep(bool nested, NInt nstk) {
		auto lh = chr(nested, nstk);
		if (std::get<minrx_result_t>(lh))
			return lh;
		bool optional = false, infinite = false;
		for (;;) {
			auto wc = wconv.look();
			switch (wc) {
			case L'?':
				optional = true;
				wconv.nextchr();
				continue;
			case L'*':
				optional = infinite = true;
				wconv.nextchr();
				continue;
			case L'+':
				infinite = true;
				wconv.nextchr();
				continue;
			case L'{':
				if ((flags & MINRX_REG_BRACE_COMPAT) == 0
				    || (enc == WConv::Encoding::Byte ? std::isdigit(wconv.lookahead())
								     : std::iswdigit(wconv.lookahead())))
				{
					if (optional || infinite) {
						lh = mkrep(lh, optional, infinite, nstk);
						optional = infinite = false;
					}
					wc = wconv.nextchr().look();
					if (wc == WConv::End)
						return {{}, 0, MINRX_REG_EBRACE};
					NInt m, n;
					if (!num(m))
						return {{}, 0, MINRX_REG_BADBR};
					wc = wconv.look();
					if (wc == L'}') {
						lh = mkrep(lh, m, m, nstk);
						wconv.nextchr();
						continue;
					}
					if (wc == WConv::End)
						return {{}, 0, MINRX_REG_EBRACE};
					if (wc != L',')
						return {{}, 0, MINRX_REG_BADBR};
					wc = wconv.nextchr().look();
					if (wc == L'}') {
						lh = mkrep(lh, m, -1, nstk);
						wconv.nextchr();
						continue;
					}
					if (!num(n))
						return {{}, 0, MINRX_REG_BADBR};
					wc = wconv.look();
					if (wc == WConv::End)
						return {{}, 0, MINRX_REG_EBRACE};
					if (wc != L'}')
						return {{}, 0, MINRX_REG_BADBR};
					lh = mkrep(lh, m, n, nstk);
					wconv.nextchr();
					continue;
				}
				// fall through
			default:
				if (optional || infinite) {
					lh = mkrep(lh, optional, infinite, nstk);
					optional = infinite = false;
				}
				return lh;
			}
		}
	}
	Subexp chr(bool nested, NInt nstk) {
		std::deque<Node> lhs;
		std::size_t lhmaxstk;
		auto wc = wconv.look();
		switch (wc) {
		default:
		normal:
			lhmaxstk = nstk;
			if ((flags & MINRX_REG_ICASE) == 0) {
				lhs.push_back({(NInt) wc, {0, 0}, nstk});
			} else {
				WChar wcl = enc == WConv::Encoding::Byte ? std::tolower(wc) : std::towlower(wc);
				WChar wcu = enc == WConv::Encoding::Byte ? std::toupper(wc) : std::towupper(wc);
				if (wc != wcl || wc != wcu) {
					auto key = std::min(wc, std::min(wcl, wcu));
					if (icmap.find(key) == icmap.end()) {
						icmap.emplace(key, csets.size());
						csets.emplace_back(enc);
						csets.back().set(wc);
						csets.back().set(wcl);
						csets.back().set(wcu);
					}
					lhs.push_back({Node::CSet, {icmap[key], 0}, nstk});
				} else {
					lhs.push_back({(NInt) wc, {0, 0}, nstk});
				}
			}
			wconv.nextchr();
			break;
		case L'{':
			if ((flags & MINRX_REG_BRACE_COMPAT) != 0
			    && (enc == WConv::Encoding::Byte ? !std::isdigit(wconv.lookahead())
							     : !std::iswdigit(wconv.lookahead())))
				goto normal;
			// fall through
		case L'*':
		case L'+':
		case L'?':
			return {{}, 0, MINRX_REG_BADRPT};
		case L'[':
			lhmaxstk = nstk;
			lhs.push_back({Node::CSet, {csets.size(), 0}, nstk});
			if (auto err = csets.emplace_back(enc).parse(flags, enc, wconv))
				return {{}, 0, err};
			break;
		case L'.':
			if (!dot.has_value()) {
				dot = csets.size();
				csets.emplace_back(enc);
				if ((flags & MINRX_REG_NEWLINE) != 0)
					csets.back().set(L'\n');
				csets.back().invert();
			}
			lhmaxstk = nstk;
			lhs.push_back({Node::CSet, {*dot, 0}, nstk});
			wconv.nextchr();
			break;
		case L'^':
			lhmaxstk = nstk;
			lhs.push_back({(flags & MINRX_REG_NEWLINE) == 0 ? Node::ZBOB : Node::ZBOL, {0, 0}, nstk});
			wconv.nextchr();
			break;
		case L'$':
			lhmaxstk = nstk;
			lhs.push_back({(flags & MINRX_REG_NEWLINE) == 0 ? Node::ZEOB : Node::ZEOL, {0, 0}, nstk});
			wconv.nextchr();
			break;
		case L'\\':
			lhmaxstk = nstk;
			wc = wconv.nextchr().look();
			switch (wc) {
			case L'<':
				if ((flags & MINRX_REG_EXTENSIONS_BSD) == 0)
					goto normal;
				lhs.push_back({Node::ZBOW, {0, 0}, nstk});
				break;
			case L'>':
				if ((flags & MINRX_REG_EXTENSIONS_BSD) == 0)
					goto normal;
				lhs.push_back({Node::ZEOW, {0, 0}, nstk});
				break;
			case L'`':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				lhs.push_back({Node::ZBOB, {0, 0}, nstk});
				break;
			case L'\'':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				lhs.push_back({Node::ZEOB, {0, 0}, nstk});
				break;
			case L'b':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				lhs.push_back({Node::ZXOW, {0, 0}, nstk});
				break;
			case L'B':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				lhs.push_back({Node::ZNWB, {0, 0}, nstk});
				break;
			case L's':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_s.has_value()) {
					esc_s = csets.size();
					WConv wc(enc, "[[:space:]]");
					csets.emplace_back(enc).parse(flags, enc, wc.nextchr());
				}
				lhs.push_back({Node::CSet, {*esc_s, 0}, nstk});
				break;
			case L'S':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_S.has_value()) {
					esc_S = csets.size();
					WConv wc(enc, "[^[:space:]]");
					csets.emplace_back(enc).parse(flags, enc, wc.nextchr());
				}
				lhs.push_back({Node::CSet, {*esc_S, 0}, nstk});
				break;
			case L'w':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_w.has_value()) {
					esc_w = csets.size();
					WConv wc(enc, "[[:alnum:]_]");
					csets.emplace_back(enc).parse(flags, enc, wc.nextchr());
				}
				lhs.push_back({Node::CSet, {*esc_w, 0}, nstk});
				break;
			case L'W':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_W.has_value()) {
					esc_W = csets.size();
					WConv wc(enc, "[^[:alnum:]_]");
					csets.emplace_back(enc).parse(flags, enc, wc.nextchr());
				}
				lhs.push_back({Node::CSet, {*esc_W, 0}, nstk});
				break;
			case WConv::End:
				return {{}, 0, MINRX_REG_EESCAPE};
			default:
				goto normal;
			}
			wconv.nextchr();
			break;
		case L'(':
			{
				NInt n = ++nsub;
				wconv.nextchr();
				minrx_result_t err;
				std::tie(lhs, lhmaxstk, err) = alt(true, nstk + 1);
				if (err)
					return {lhs, lhmaxstk, err};
				if (wconv.look() != L')')
					return {{}, 0, MINRX_REG_EPAREN};
				lhs.push_front({Node::SubL, {n, nsub}, nstk + 1});
				lhs.push_back({Node::SubR, {n, nsub}, nstk});
				wconv.nextchr();
			}
			break;
		case L')':
			if (!nested)
				goto normal;
			// fall through
		case L'|':
		case WConv::End:
			lhmaxstk = nstk;
			break;
		}
		return {lhs, lhmaxstk, MINRX_REG_SUCCESS};
	}
	Regexp *compile() {
		auto [lhs, nstk, err] = alt(false, 0);
		if (err) {
			csets.clear();
			lhs.clear();
			nstk = 0;
			nsub = 0;
		} else {
			lhs.push_back({Node::Exit, {0, 0}, 0});
		}
		return new Regexp{ std::move(csets), {lhs.begin(), lhs.end()}, nstk, nsub + 1, enc, err };
	}
};

struct Execute {
	typedef COWVec<std::size_t, (std::size_t) -1> Vec;
	struct NState {
		std::size_t gen = 0;
		std::size_t boff;
		Vec substack;
		NState() {}
		NState(Vec::Allocator &allocator): substack(allocator) {}
		template <typename... XArgs>
		bool cmpgt(const NState &ns, std::size_t gen, std::size_t nstk, XArgs... xargs) const {
			if (gen != ns.gen)
				return gen > ns.gen;
			if (boff != ns.boff)
				return boff < ns.boff;
			return substack.cmpgt(ns.substack, nstk, xargs...);
		}
	};
	const Regexp &r;
	const minrx_regexec_flags_t flags;
	std::size_t gen = 0;
	WConv wconv;
	WChar wcprev = WConv::End;
	Vec::Allocator allocator { r.nstk + 2 * r.nsub };
	std::optional<COWVec<std::size_t, (std::size_t) -1>> best;
	QSet<NInt> epsq { r.nodes.size() };
	QVec<NInt, NState> epsv { r.nodes.size() };
	const Node *nodes = r.nodes.data();
	Execute(const Regexp &r, minrx_regexec_flags_t flags, const char *bp, const char *ep) : r(r), flags(flags), wconv(r.enc, bp, ep) {}
	template <typename... XArgs>
	INLINE
	void add(QVec<NInt, NState> &ncsv, NInt k, NInt nstk, const NState &ns, WChar wcnext, XArgs... xargs) {
		const Node &n = nodes[k];
		if (n.type <= Node::CSet) {
			if (n.type == (NInt) wcnext || (n.type == Node::CSet && r.csets[n.args[0]].test(wcnext))) {
				auto [newly, newns] = ncsv.insert(k, ns);
				if (newly)
					new (&newns) NState(ns);
				else if (ns.cmpgt(newns, gen, nstk, xargs...))
					newns = ns;
				else
					return;
				newns.gen = gen;
				if constexpr (sizeof... (XArgs) > 0) {
					auto i = nstk - sizeof...(XArgs);
					(newns.substack.put(i++, xargs), ...);
				}
			}
		} else {
			auto [newly, newns] = epsv.insert(k, ns);
			if (newly)
				new (&newns) NState(ns);
			else if (ns.cmpgt(newns, gen, nstk, xargs...))
				newns = ns;
			else
				return;
			newns.gen = gen;
			if constexpr (sizeof... (XArgs) > 0) {
				auto i = nstk - sizeof...(XArgs);
				(newns.substack.put(i++, xargs), ...);
			}
			epsq.insert(k);
		}
	}
	void epsclosure(QVec<NInt, NState> &ncsv, WChar wcnext) {
		auto nodes = this->nodes;
		auto is_word = r.enc == WConv::Encoding::Byte ? [](WChar b) { return b == '_' || std::isalnum(b); }
							      : [](WChar wc) { return wc == L'_' || std::iswalnum(wc); };
		do {
			NInt k = epsq.remove();
			NState &ns = epsv.lookup(k);
			if (best.has_value() && ns.boff > best->get(r.nstk + 0))
				continue;
			const auto &n = nodes[k];
			auto nstk = n.nstk;
			switch (n.type) {
			case Node::Exit:
				{
					auto b = ns.boff, e = wconv.off();
					if (!best.has_value()
					    || b < best->get(r.nstk + 0)
					    || (b == best->get(r.nstk + 0) && e >= best->get(r.nstk + 1)))
					{
						best = ns.substack;
						best->put(r.nstk + 0, b);
						best->put(r.nstk + 1, e);
					}
				}
				break;
			case Node::Fork:
				{
					NInt priority = (NInt) -1;
					do {
						add(ncsv, k + 1, nstk, ns, wcnext, priority--);
						k = k + 1 + nodes[k].args[0];
					} while (nodes[k].type != Node::Join);
				}
				break;
			case Node::Goto:
				add(ncsv, k + 1 + n.args[1], nstk, ns, wcnext);
				break;
			case Node::Join:
				add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::Loop:
				add(ncsv, k + 1, nstk, ns, wcnext, (NInt) wconv.off(), (NInt) -1, (NInt) wconv.off());
				if (n.args[1])
					add(ncsv, k + 1 + n.args[0], nstk, ns, wcnext, (NInt) wconv.off(), (NInt) 0, (NInt) wconv.off());
				break;
			case Node::Next:
				add(ncsv, k + 1, nstk, ns, wcnext);
				if (n.args[1] && wconv.off() > ns.substack.get(nstk + 3 - 1))
					add(ncsv, k - n.args[0], nstk + 3, ns, wcnext, ns.substack.get(nstk), ns.substack.get(nstk + 1) - 1, (NInt) wconv.off());
				break;
			case Node::Skip:
				add(ncsv, k + 1, nstk, ns, wcnext, (NInt) 0);
				add(ncsv, k + 1 + n.args[0], nstk, ns, wcnext, (NInt) 1);
				break;
			case Node::SubL:
				{
					NState nscopy = ns;
					nscopy.substack.put(nstk - 1, wconv.off());
					if (n.args[0] != (NInt) -1)
						for (auto i = n.args[0]; i <= n.args[1]; ++i) {
							nscopy.substack.put(r.nstk + i * 2, -1);
							nscopy.substack.put(r.nstk + i * 2 + 1, -1);
						}
					add(ncsv, k + 1, nstk, nscopy, wcnext);
				}
				break;
			case Node::SubR:
				if (n.args[0] != (NInt) -1) {
					NState nscopy = ns;
					nscopy.substack.put(r.nstk + n.args[0] * 2 + 0, ns.substack.get(nstk));
					nscopy.substack.put(r.nstk + n.args[0] * 2 + 1, wconv.off());
					add(ncsv, k + 1, nstk, nscopy, wcnext);
				} else {
					add(ncsv, k + 1, nstk, ns, wcnext);
				}
				break;
			case Node::ZBOB:
				if (wconv.off() == 0 && (flags & MINRX_REG_NOTBOL) == 0)
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZEOB:
				if (wconv.look() == WConv::End && (flags & MINRX_REG_NOTEOL) == 0)
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZBOL:
				if (((wconv.off() == 0 && (flags & MINRX_REG_NOTBOL) == 0)) || wcprev == L'\n')
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZEOL:
				if (((wconv.look() == WConv::End && (flags & MINRX_REG_NOTEOL) == 0)) || wconv.look() == L'\n')
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZBOW:
				if ((wconv.off() == 0 || !is_word(wcprev)) && (wconv.look() != WConv::End && is_word(wconv.look())))
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZEOW:
				if ((wconv.off() != 0 && is_word(wcprev)) && (wconv.look() == WConv::End || !is_word(wconv.look())))
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZXOW:
				if (   ((wconv.off() == 0 || !is_word(wcprev)) && (wconv.look() != WConv::End && is_word(wconv.look())))
				    || ((wconv.off() != 0 && is_word(wcprev)) && (wconv.look() == WConv::End || !is_word(wconv.look()))))
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZNWB:
				if (   (wconv.off() == 0 && wconv.look() == WConv::End)
				    || (wconv.off() == 0 && wconv.look() != WConv::End && !is_word(wconv.look()))
				    || (wconv.off() != 0 && !is_word(wcprev) && wconv.look() == WConv::End)
				    || (wconv.off() != 0 && wconv.look() != WConv::End && is_word(wcprev) == is_word(wconv.look())))
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			default:
				abort();
				break;
			}
		} while (!epsq.empty());
	}
	int execute(std::size_t nm, minrx_regmatch_t *rm) {
		QVec<NInt, NState> mcsvs[2] { r.nodes.size(), r.nodes.size() };
		wconv.nextchr();
		if ((flags & MINRX_REG_RESUME) != 0 && rm && rm[0].rm_eo > 0)
			while (wconv.look() != WConv::End && (std::ptrdiff_t) wconv.off() < rm[0].rm_eo)
				wcprev = wconv.look(), wconv.nextchr();
		NState nsinit(allocator);
		nsinit.boff = wconv.off();
		auto wcnext = wconv.look();
		add(mcsvs[0], 0, 0, nsinit, wcnext);
		if (!epsq.empty())
			epsclosure(mcsvs[0], wcnext);
		for (;;) { // unrolled to ping-pong roles of mcsvs[0]/[1]
			if (wcnext == WConv::End)
				break;
			++gen;
			wcprev = wcnext, wcnext = wconv.nextchr().look();
			while (!mcsvs[0].empty()) {
				auto [n, ns] = mcsvs[0].remove();
				add(mcsvs[1], n + 1, nodes[n].nstk, ns, wcnext);
			}
			if (!best.has_value()) {
				nsinit.boff = wconv.off();
				add(mcsvs[1], 0, 0, nsinit, wcnext);
			}
			if (!epsq.empty())
				epsclosure(mcsvs[1], wcnext);
			if (best.has_value() && mcsvs[1].empty())
				break;
			if (wcnext == WConv::End)
				break;
			++gen;
			wcprev = wcnext, wcnext = wconv.nextchr().look();
			while (!mcsvs[1].empty()) {
				auto [n, ns] = mcsvs[1].remove();
				add(mcsvs[0], n + 1, nodes[n].nstk, ns, wcnext);
			}
			if (!best.has_value()) {
				nsinit.boff = wconv.off();
				add(mcsvs[0], 0, 0, nsinit, wcnext);
			}
			if (!epsq.empty())
				epsclosure(mcsvs[0], wcnext);
			if (best.has_value() && mcsvs[0].empty())
				break;
		}
		if (best.has_value()) {
			if (rm) {
				std::size_t nsub = std::min(nm, r.nsub);
				std::size_t i;
				for (i = 0; i < nsub; ++i) {
					rm[i].rm_so = (*best->storage)[r.nstk + i * 2];
					rm[i].rm_eo = (*best->storage)[r.nstk + i * 2 + 1];
				}
				for (; i < nm; ++i)
					rm[i].rm_so = rm[i].rm_eo = -1;
			}
			return 0;
		} else {
			if (rm)
				for (std::size_t i = 0; i < nm; ++i)
					rm[i].rm_so = rm[i].rm_eo = -1;
			return MINRX_REG_NOMATCH;
		}
	}
};

}

int
minrx_regcomp(minrx_regex_t *rx, const char *s, int flags)
{
	return minrx_regncomp(rx, strlen(s), s, flags);
}

int
minrx_regexec(minrx_regex_t *rx, const char *s, std::size_t nm, minrx_regmatch_t *rm, int flags)
{
	return minrx_regnexec(rx, strlen(s), s, nm, rm, flags);
}

int
minrx_regncomp(minrx_regex_t *rx, std::size_t ns, const char *s, int flags)
{
	auto enc = MinRX::WConv::Encoding::MBtoWC;
	auto loc = std::setlocale(LC_CTYPE, nullptr);
	if ((loc != nullptr && loc[0] == 'C' && loc[1] == '\0') || ((flags & MINRX_REG_NATIVE1B) != 0 && MB_CUR_MAX == 1))
		enc = MinRX::WConv::Encoding::Byte;
	else if (auto utf = std::strchr(loc ? loc : "", '.');
		 utf != nullptr && (utf[1] == 'U' || utf[1] == 'u')
				&& (utf[2] == 'T' || utf[2] == 't')
				&& (utf[3] == 'F' || utf[3] == 'f')
				&& (   (utf[4] == '8' && utf[5] == '\0')
				    || (utf[4] == '-' && utf[5] == '8' && utf[6] == '\0')))
		enc = MinRX::WConv::Encoding::UTF8;
	auto r = MinRX::Compile(enc, s, s + ns, (minrx_regcomp_flags_t) flags).compile();
	rx->re_regexp = r;
	rx->re_nsub = r->nsub - 1;
	rx->re_compflags = (minrx_regcomp_flags_t) flags;
	return r->err;
}

int
minrx_regnexec(minrx_regex_t *rx, std::size_t ns, const char *s, std::size_t nm, minrx_regmatch_t *rm, int flags)
{
	MinRX::Regexp *r = reinterpret_cast<MinRX::Regexp *>(rx->re_regexp);
	return MinRX::Execute(*r, (minrx_regexec_flags_t) flags, s, s + ns).execute(nm, rm);
}

void
minrx_regfree(minrx_regex_t *rx)
{
	delete reinterpret_cast<MinRX::Regexp *>(rx->re_regexp);
	rx->re_regexp = nullptr;
}

size_t
minrx_regerror(int errcode, const minrx_regex_t *, char *errbuf, size_t errsize)
{
	static const char *const messages[] = {
		N_("success"),
		N_("bad pattern"),
		N_("invalid contents of {}"),
		N_("? * + or {interval} not preceded by valid subpattern"),
		N_("unbalanced {"),
		N_("unbalanced ["),
		N_("invalid collating element"),
		N_("invalid character class name"),
		N_("invalid trailing backslash"),
		N_("unbalanced ("),
		N_("invalid range endpoint"),
		N_("memory allocation failed"),
		N_("invalid \\digit"),
		N_("match not found"),
		N_("unknown error code"),
	};
	if (errcode < 0 || errcode > MINRX_REG_UNKNOWN)
		errcode = MINRX_REG_UNKNOWN;
	size_t size = snprintf(errbuf, errsize, "%s", _(messages[errcode]));
	if (errsize != 0 && size == errsize)
		errbuf[errsize - 1] = '\0';
	return size + 1;
}
