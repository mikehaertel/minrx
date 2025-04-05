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
	auto cmp(std::size_t o, std::size_t n, const COWVec &other) const {
		const TYPE *xv = &(*storage)[0];
		const TYPE *yv = &(*other.storage)[0];
		for (std::size_t i = 0; i < n; i++)
			if (xv[o + i] != yv[o + i])
				return xv[o + i] <=> yv[o + i];
		return (TYPE) 0 <=> (TYPE) 0;
	}
	template <typename... XArgs>
	auto cmp(const COWVec &other, std::size_t limit, XArgs... xargs) const {
		std::size_t i;
		const TYPE *xv = &(*storage)[0];
		const TYPE *yv = &(*other.storage)[0];
		for (i = 0; i < limit - sizeof... (XArgs); i++)
			if (xv[i] != yv[i])
				return xv[i] <=> yv[i];
		if constexpr (sizeof...(XArgs) > 0)
			for (TYPE x : { xargs... })
				if (x != yv[i++])
					return x <=> yv[i - 1];
		return (TYPE) 0 <=> (TYPE) 0;
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
	std::uint64_t bits0;
	std::uint64_t *bitsfree = nullptr;
	int depth = 0;
	QSet(UINT limit) {
		std::size_t s[10], t = 0;
		do
			t += (limit = s[depth++] = (limit + 63u) / 64u);
		while (limit > 1);
		std::uint64_t *next = bitsfree = (std::uint64_t *) ::operator new(t * sizeof (std::uint64_t));
		bits[0] = &bits0;
		for (int i = 1; i < depth; ++i)
			bits[i] = next, next += s[depth - 1 - i];
		bits0 = 0;
	}
	~QSet() { if (bitsfree) ::operator delete(bitsfree); }
	static std::uint64_t bit(UINT k) { return (std::uint64_t) 1 << (k & 0x3F); }
	bool empty() const { return !bits0; }
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
	std::tuple<bool, DATA&> insert(UINT k, const DATA&) {
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
class WConv final {
public:
	enum { End = -1 };
	enum class Encoding { Byte, MBtoWC, UTF8 };
private:
	WChar (WConv::*const nextfn)();
	const char *const bp;
	const char *const ep;
	const char *cp;
	std::mbstate_t mbs;
	static WChar (WConv::*const nextfns[])();
public:
	WConv(const WConv &) = default;
	WConv(Encoding e, const char *bp, const char *ep)
	: nextfn(nextfns[(int) e]), bp(bp), ep(ep), cp(bp) {
		std::memset(&mbs, 0, sizeof mbs);
	}
	WConv(Encoding e, const char *bp): WConv(e, bp, bp + std::strlen(bp)) { }
	auto lookahead() const { return WConv(*this).nextchr(); }
	WChar nextchr() { return (this->*nextfn)(); }
	WChar nextbyte() { return cp != ep ? (unsigned char) *cp++ : (WChar) End; }
	WChar nextmbtowc() {
		wchar_t wct = L'\0';
		if (cp != ep) {
			auto n = mbrtowc(&wct, cp, ep - cp, &mbs);
			if (n == 0 || n == (std::size_t) -1 || n == (std::size_t) -2) {
				if (wct == L'\0')
					wct = std::numeric_limits<WChar>::min() + (unsigned char) *cp++;
			} else {
				cp += n;
			}
			return wct;
		} else {
			return End;
		}
	}
	WChar nextutf8() {
		if (cp != ep) {
			WChar u = (unsigned char) cp[0];
			if (u < 0x80)
				return cp += 1, u;
			if ((u & 0x40) == 0 || cp + 1 == ep)
			error:
				return cp += 1, std::numeric_limits<WChar>::min() + u;
			WChar v = (unsigned char) cp[1];
			if ((v & 0xC0) != 0x80)
				goto error;
			if ((u & 0x20) == 0) {
				WChar r = ((u & 0x1F) << 6) | (v & 0x3F);
				if (r < 0x80)
					goto error;
				return cp += 2, r;
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
				return cp += 3, r;
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
			return cp += 4, r;
		} else {
			return End;
		}
	}
	std::size_t off() const { return cp - bp; }
	auto ptr() const { return cp; }
	auto save() { return cp; }
	void restore(const char *p) { cp = p; }
};

WChar (WConv::*const WConv::nextfns[3])() = { &WConv::nextbyte, &WConv::nextmbtowc, &WConv::nextutf8 };

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
		auto wc = wconv.nextchr();
		bool inv = wc == L'^';
		if (inv)
			wc = wconv.nextchr();
		for (bool first = true; first || wc != L']'; first = false) {
			if (wc == WConv::End)
				return MINRX_REG_EBRACK;
			auto wclo = wc, wchi = wc;
			wc = wconv.nextchr();
			if (wclo == L'\\' && (flags & MINRX_REG_BRACK_ESCAPE) != 0) {
				if (wc != WConv::End) {
					wclo = wchi = wc;
					wc = wconv.nextchr();
				} else {
					return MINRX_REG_EESCAPE;
				}
			} else if (wclo == L'[') {
				if (wc == L'.') {
					wc = wconv.nextchr();
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
					wc = wconv.nextchr();
					if (wc != L'.' || (wc = wconv.nextchr()) != L']')
						return MINRX_REG_ECOLLATE;
					wc = wconv.nextchr();
				} else if (wc == L':') {
					auto bp = wconv.ptr(), ep = bp;
					do
						ep = wconv.ptr(), wc = wconv.nextchr();
					while (wc != WConv::End && wc != L':');
					if (wc != L':')
						return MINRX_REG_ECTYPE;
					wc = wconv.nextchr();
					if (wc != L']')
						return MINRX_REG_ECTYPE;
					wc = wconv.nextchr();
					auto cclname = std::string(bp, ep);
					if (cclass(flags, enc, cclname))
						continue;
					return MINRX_REG_ECTYPE;
				} else if (wc == L'=') {
					wc = wconv.nextchr();
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
					wc = wconv.nextchr();
					if (wc != L'=' || (wc = wconv.nextchr()) != L']')
						return MINRX_REG_ECOLLATE;
				}
			}
			bool range = false;
			if (wc == L'-') {
				auto save = wconv.save();
				wc = wconv.nextchr();
				if (wc == WConv::End)
					return MINRX_REG_EBRACK;
				if (wc != L']') {
					wchi = wc;
					wc = wconv.nextchr();
					if (wchi == L'\\' && (flags & MINRX_REG_BRACK_ESCAPE) != 0) {
						if (wc != WConv::End) {
							wchi = wc;
							wc = wconv.nextchr();
						} else {
							return MINRX_REG_EESCAPE;
						}
					} else if (wchi == L'[') {
						if (wc == L'.') {
							wchi = wconv.nextchr();
							wc = wconv.nextchr();
							if (wc != L'.' || (wc = wconv.nextchr()) != L']')
								return MINRX_REG_ECOLLATE;
							wc = wconv.nextchr();
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
		MinB,			// args = this minified subexpression nesting depth
		MinL,			// args = this minified subexpression nesting depth
		MinR,			// args = this minified subexpression nesting depth
		MinX,			// args = this minified subexpression nesting depth
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
	WConv::Encoding enc;
	minrx_result_t err;
	const std::vector<CSet> csets;
	const std::vector<Node> nodes;
	std::size_t nmin;
	std::size_t nstk;
	std::size_t nsub;
	bool minglobal;
};

struct Compile {
	const minrx_regcomp_flags_t flags;
	WConv::Encoding enc;
	WConv wconv;
	WChar wc;
	std::vector<CSet> csets;
	std::optional<std::size_t> dot;
	std::optional<std::size_t> esc_s;
	std::optional<std::size_t> esc_S;
	std::optional<std::size_t> esc_w;
	std::optional<std::size_t> esc_W;
	std::map<WChar, unsigned int> icmap;
	NInt nmin = 0;
	NInt nsub = 0;
	bool minglobal = (flags & MINRX_REG_MINGLOBAL) != 0;
	Compile(WConv::Encoding e, const char *bp, const char *ep, minrx_regcomp_flags_t flags): flags(flags), enc(e), wconv(e, bp, ep) { wc = wconv.nextchr(); }
	bool num(NInt &n, WChar &wc) {
		auto satmul = [](NInt x, NInt y) -> NInt {
			return (x == 0 || y == 0) ? 0 : ((x * y / x == y) ? x * y : -1);
		};
		if (wc < L'0' || wc > L'9')
			return false;
		NInt v = 0;
		do {
			v = satmul(v, 10);
			if (v == (NInt) -1 || (v += wc - L'0') < (NInt) wc - L'0') {
				do
					wc = wconv.nextchr();
				while (wc >= L'0' && wc <= L'9');
				n = -1;
				return true;
			}
			wc = wconv.nextchr();
		} while (wc >= L'0' && wc <= L'9');
		n = v;
		return true;
	}
	enum MinState { Unused, Exited, Active };
	typedef std::tuple<std::deque<Node>, std::size_t, MinState, minrx_result_t> Subexp;
	Subexp alt(bool nested, NInt nstk) {
		auto [lhs, lhmaxstk, lminstate, err] = cat(nested, nstk);
		if (err)
			return {lhs, lhmaxstk, lminstate, err};
		if (wc == L'|') {
			for (auto &l : lhs)
				l.nstk += 1;
			std::vector<Subexp> alts;
			while (wc == L'|') {
				wc = wconv.nextchr();
				alts.push_back(cat(nested, nstk + 1));
			}
			auto [rhs, rhmaxstk, rminstate, err] = alts.back();
			if (err)
				return {rhs, rhmaxstk, rminstate, err};
			rhs.push_front({Node::Goto, {rhs.size(), rhs.size() + 1}, nstk + 1});
			alts.pop_back();
			while (!alts.empty()) {
				auto [mhs, mhmaxstk, mminstate, _] = alts.back();
				alts.pop_back();
				rhs.insert(rhs.begin(), mhs.begin(), mhs.end());
				rhmaxstk = std::max(mhmaxstk, rhmaxstk);
				rminstate = std::max(mminstate, rminstate);
				rhs.push_front({Node::Goto, {mhs.size(), rhs.size() + 1}, nstk + 1});
			}
			lhs.push_front({Node::Fork, { lhs.size(), 0 }, nstk + 1});
			lhs.insert(lhs.end(), rhs.begin(), rhs.end());
			lhmaxstk = std::max(lhmaxstk, rhmaxstk);
			lminstate = std::max(lminstate, rminstate);
			lhs.push_back({Node::Join, {lhs.size() - 1, 0}, nstk + 1});
		}
		return {lhs, lhmaxstk, lminstate, MINRX_REG_SUCCESS};
	}
	Subexp cat(bool nested, NInt nstk) {
		auto [lhs, lhmaxstk, lminstate, err] = rep(nested, nstk);
		if (err)
			return {lhs, lhmaxstk, lminstate, err};
		while (wc != WConv::End && wc != L'|' && (wc != L')' || !nested)) {
			auto [rhs, rhmaxstk, rminstate, err] = rep(nested, nstk);
			if (err)
				return {rhs, rhmaxstk, rminstate, err};
			lhs.insert(lhs.end(), rhs.begin(), rhs.end());
			lhmaxstk = std::max(lhmaxstk, rhmaxstk);
			lminstate = rminstate ? rminstate : lminstate;
		}
		return {lhs, lhmaxstk, lminstate, MINRX_REG_SUCCESS};
	}
	Subexp minimize(const Subexp &lh, NInt nstk) {
		auto [nodes, maxstk, minstate, err] = lh;
		for (auto &n : nodes)
			n.nstk += 1;
		nodes.push_front({Node::MinL, {0, 0}, nstk + 1});
		nodes.push_back({Node::MinR, {0, 0}, nstk});
		nmin = std::max(nmin, (std::size_t) 1);
		return {nodes, maxstk + 1, Active, err};
	}
	void minraise(Subexp &lh) {
		auto &[nodes, maxstk, minstate, err] = lh;
		NInt maxlevel = 0;
		for (auto &n : nodes)
			switch (n.type) {
			case Node::MinB:
			case Node::MinL:
			case Node::MinR:
			case Node::MinX:
				maxlevel = std::max(maxlevel, ++n.args[0]);
				break;
			default:
				;
			}
		nmin = std::max(nmin, maxlevel + 1);
	}
	Subexp mkrep(const Subexp &lh, bool optional, bool infinite, NInt nstk) {
		auto [lhs, lhmaxstk, lminstate, _] = lh;
		if (optional && !infinite) {
			for (auto &l : lhs) l.nstk += 2;
			auto lhsize = lhs.size();
			lhs.push_front({Node::Skip, {lhsize, 0}, nstk + 2});
			return {lhs, lhmaxstk + 2, lminstate, MINRX_REG_SUCCESS};
		} else {
			for (auto &l : lhs) l.nstk += 3;
			auto lhsize = lhs.size();
			lhs.push_front({Node::Loop, {lhsize, (NInt) optional}, nstk + 3});
			lhs.push_back({Node::Next, {lhsize, (NInt) infinite}, nstk});
			return {lhs, lhmaxstk + 3, lminstate, MINRX_REG_SUCCESS};
		}
	}
	Subexp mkrep(const Subexp &lh, NInt m, NInt n, NInt nstk) {
		if ((m != (NInt) -1 && m > RE_DUP_MAX) || (n != (NInt) -1 && n > RE_DUP_MAX) || m > n)
			return {{}, 0, Unused, MINRX_REG_BADBR};
		if (n == 0)
			return {{}, 0, Unused, MINRX_REG_SUCCESS};
		if (m == 0 && n == 1)
			return mkrep(lh, true, false, nstk);
		if (m == 0 && n == (NInt) -1)
			return mkrep(lh, true, true, nstk);
		if (m == 1 && n == 1)
			return lh;
		if (m == 1 && n == (NInt) -1)
			return mkrep(lh, false, true, nstk);
		auto [lhs, lhmaxstk, lminstate, _] = lh;
		auto [rhs, rhmaxstk, rminstate,__] = lh;
		NInt k;
		for (k = 1; k < m; ++k)
			lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		if (n != (NInt) -1 && k < n) {
			lhmaxstk += 1;
			rhmaxstk += 1;
			for (auto &r : rhs)
				r.nstk += 2;
			auto rhsize = rhs.size();
			rhs.push_front({Node::Skip, {rhsize, 1}, nstk + 2});
			for (; k < n; ++k)
				lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		}
		if (n == (NInt) -1) {
			lhmaxstk += 3;
			rhmaxstk += 3;
			for (auto &r : rhs)
				r.nstk += 3;
			auto rhsize = rhs.size();
			rhs.push_front({Node::Loop, {rhsize, 1}, nstk + 3});
			rhs.push_back({Node::Next, {rhsize, 1}, nstk});
			lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		}
		if (m == 0)
			return mkrep({lhs, rhmaxstk, rminstate, MINRX_REG_SUCCESS}, true, false, nstk);
		else
			return {lhs, rhmaxstk, rminstate, MINRX_REG_SUCCESS};
	}
	Subexp rep(bool nested, NInt nstk) {
		auto lh = chr(nested, nstk);
		if (std::get<minrx_result_t>(lh))
			return lh;
		auto minstate = std::get<MinState>(lh);
		for (;;) {
			bool infinite = false, minimal = (flags & MINRX_REG_MINIMAL) != 0, optional = false;
			switch (wc) {
			case L'?':
				optional = true;
				goto common;
			case L'*':
				infinite = optional = true;
				goto common;
			case L'+':
				infinite = true;
			common:	if ((flags & MINRX_REG_MINDISABLE) == 0 && (wc = wconv.nextchr()) == L'?')
					minimal ^= true, wc = wconv.nextchr();
				if ((flags & MINRX_REG_RPTMINSLOW) == 0 && (minstate == Active || (minglobal && minstate == Exited))) {
					std::get<0>(lh).push_back({Node::MinX, {0, 0}, nstk});
					std::get<MinState>(lh) = minstate = Exited;
				}
				if (minstate != Unused && (minglobal || minimal)) {
					minraise(lh);
					if (!minglobal)
						std::get<MinState>(lh) = minstate = Unused;
				}
				lh = mkrep(minimal ? minimize(lh, nstk) : lh, optional, infinite, nstk);
			comout:	if (minimal)
					std::get<0>(lh).push_front({Node::MinB, {0, 0}, nstk});
				if ((flags & MINRX_REG_RPTMINSLOW) != 0 && (minstate == Active || (minglobal && minstate == Exited))) { // N.B. pre-mkrep minstate
					NInt raised = minglobal | minimal;
					std::get<0>(lh).push_back({Node::MinX, {raised, 0}, nstk});
					if (!minimal)
						std::get<MinState>(lh) = Exited;
				}
				minstate = std::get<MinState>(lh);
				continue;
			case L'{':
				if ((flags & MINRX_REG_BRACE_COMPAT) == 0
				    || (enc == WConv::Encoding::Byte ? std::isdigit(wconv.lookahead())
								     : std::iswdigit(wconv.lookahead())))
				{
					wc = wconv.nextchr();
					if (wc == WConv::End)
						return {{}, 0, Unused, MINRX_REG_EBRACE};
					NInt m, n;
					if (!num(m, wc))
						return {{}, 0, Unused, MINRX_REG_BADBR};
					if (wc == L'}') {
						if ((flags & MINRX_REG_MINDISABLE) == 0 && (wc = wconv.nextchr()) == L'?')
							minimal ^= true, wc = wconv.nextchr();
						if ((flags & MINRX_REG_RPTMINSLOW) == 0 && (minstate == Active || (minglobal && minstate == Exited))) {
							std::get<0>(lh).push_back({Node::MinX, {0, 0}, nstk});
							std::get<MinState>(lh) = minstate = Exited;
						}
						if (minstate != Unused && (minglobal || minimal)) {
							minraise(lh);
							if (!minglobal)
								std::get<MinState>(lh) = minstate = Unused;
						}
						lh = mkrep(minimal ? minimize(lh, nstk) : lh, m, m, nstk);
						goto comout;
					}
					if (wc == WConv::End)
						return {{}, 0, Unused, MINRX_REG_EBRACE};
					if (wc != L',')
						return {{}, 0, Unused, MINRX_REG_BADBR};
					wc = wconv.nextchr();
					if (wc == L'}') {
						if ((flags & MINRX_REG_MINDISABLE) == 0 && (wc = wconv.nextchr()) == L'?')
							minimal ^= true, wc = wconv.nextchr();
						if ((flags & MINRX_REG_RPTMINSLOW) == 0 && (minstate == Active || (minglobal && minstate == Exited))) {
							std::get<0>(lh).push_back({Node::MinX, {0, 0}, nstk});
							std::get<MinState>(lh) = minstate = Exited;
						}
						if (minstate != Unused && (minglobal || minimal)) {
							minraise(lh);
							if (!minglobal)
								std::get<MinState>(lh) = minstate = Unused;
						}
						lh = mkrep(minimal ? minimize(lh, nstk) : lh, m, -1, nstk);
						goto comout;
					}
					if (!num(n, wc))
						return {{}, 0, Unused, MINRX_REG_BADBR};
					if (wc == WConv::End)
						return {{}, 0, Unused, MINRX_REG_EBRACE};
					if (wc != L'}')
						return {{}, 0, Unused, MINRX_REG_BADBR};
					if ((flags & MINRX_REG_MINDISABLE) == 0 && (wc = wconv.nextchr()) == L'?')
						minimal ^= true, wc = wconv.nextchr();
					if ((flags & MINRX_REG_RPTMINSLOW) == 0 && (minstate == Active || (minglobal && minstate == Exited))) {
						std::get<0>(lh).push_back({Node::MinX, {0, 0}, nstk});
						std::get<MinState>(lh) = minstate = Exited;
					}
					if (minstate != Unused && (minglobal || minimal)) {
						minraise(lh);
						if (!minglobal)
							std::get<MinState>(lh) = minstate = Unused;
					}
					lh = mkrep(minimal ? minimize(lh, nstk) : lh, m, n, nstk);
					goto comout;
				}
				// fall through
			default:
				return lh;
			}
		}
	}
	Subexp chr(bool nested, NInt nstk) {
		std::deque<Node> lhs;
		std::size_t lhmaxstk;
		MinState lminstate = Unused;
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
			wc = wconv.nextchr();
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
			return {{}, 0, Unused, MINRX_REG_BADRPT};
		case L'[':
			lhmaxstk = nstk;
			lhs.push_back({Node::CSet, {csets.size(), 0}, nstk});
			if (auto err = csets.emplace_back(enc).parse(flags, enc, wconv))
				return {{}, 0, Unused, err};
			wc = wconv.nextchr();
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
			wc = wconv.nextchr();
			break;
		case L'^':
			lhmaxstk = nstk;
			lhs.push_back({(flags & MINRX_REG_NEWLINE) == 0 ? Node::ZBOB : Node::ZBOL, {0, 0}, nstk});
			wc = wconv.nextchr();
			break;
		case L'$':
			lhmaxstk = nstk;
			lhs.push_back({(flags & MINRX_REG_NEWLINE) == 0 ? Node::ZEOB : Node::ZEOL, {0, 0}, nstk});
			wc = wconv.nextchr();
			break;
		case L'\\':
			lhmaxstk = nstk;
			wc = wconv.nextchr();
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
					WConv wc(enc, "[:space:]]");
					csets.emplace_back(enc).parse(flags, enc, wc);
				}
				lhs.push_back({Node::CSet, {*esc_s, 0}, nstk});
				break;
			case L'S':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_S.has_value()) {
					esc_S = csets.size();
					WConv wc(enc, "^[:space:]]");
					csets.emplace_back(enc).parse(flags, enc, wc);
				}
				lhs.push_back({Node::CSet, {*esc_S, 0}, nstk});
				break;
			case L'w':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_w.has_value()) {
					esc_w = csets.size();
					WConv wc(enc, "[:alnum:]_]");
					csets.emplace_back(enc).parse(flags, enc, wc);
				}
				lhs.push_back({Node::CSet, {*esc_w, 0}, nstk});
				break;
			case L'W':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_W.has_value()) {
					esc_W = csets.size();
					WConv wc(enc, "^[:alnum:]_]");
					csets.emplace_back(enc).parse(flags, enc, wc);
				}
				lhs.push_back({Node::CSet, {*esc_W, 0}, nstk});
				break;
			case WConv::End:
				return {{}, 0, Unused, MINRX_REG_EESCAPE};
			default:
				goto normal;
			}
			wc = wconv.nextchr();
			break;
		case L'(':
			{
				NInt n = ++nsub;
				wc = wconv.nextchr();
				minrx_result_t err;
				std::tie(lhs, lhmaxstk, lminstate, err) = alt(true, nstk + 1);
				if (err)
					return {lhs, lhmaxstk, lminstate, err};
				if (wc != L')')
					return {{}, 0, Unused, MINRX_REG_EPAREN};
				lhs.push_front({Node::SubL, {n, nsub}, nstk + 1});
				lhs.push_back({Node::SubR, {n, nsub}, nstk});
				wc = wconv.nextchr();
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
		return {lhs, lhmaxstk, lminstate, MINRX_REG_SUCCESS};
	}
	Regexp *compile() {
		if (((flags & MINRX_REG_MINDISABLE) != 0 && (flags & (MINRX_REG_MINIMAL | MINRX_REG_MINGLOBAL | MINRX_REG_MINSCOPED | MINRX_REG_RPTMINFAST | MINRX_REG_RPTMINSLOW)) != 0)
		    || (flags & (MINRX_REG_MINGLOBAL | MINRX_REG_MINSCOPED)) == (MINRX_REG_MINGLOBAL | MINRX_REG_MINSCOPED)
		    || (flags & (MINRX_REG_RPTMINFAST | MINRX_REG_RPTMINSLOW)) == (MINRX_REG_RPTMINFAST | MINRX_REG_RPTMINSLOW))
			return new Regexp { enc, MINRX_REG_BADPAT, {}, {}, 0, 0, 1, false };
		auto [lhs, nstk, minstate, err] = alt(false, 0);
		if (err) {
			csets.clear();
			lhs.clear();
			nmin = 0;
			nstk = 0;
			nsub = 0;
		} else {
			lhs.push_back({Node::Exit, {0, 0}, 0});
		}
		if (nmin > 0)
			for (auto &l : lhs) l.nstk += nmin;
		return new Regexp { enc, err, std::move(csets), {lhs.begin(), lhs.end()}, nmin, nstk, nsub + 1, minglobal };
	}
};

struct Execute {
	constexpr static std::size_t SizeBits = std::numeric_limits<std::size_t>::digits;
	typedef COWVec<std::size_t, (std::size_t) -1> Vec;
	struct NState {
		std::size_t gen = 0;
		std::size_t boff;
		Vec substack;
		NState() {}
		NState(Vec::Allocator &allocator): substack(allocator) {}
		template <typename... XArgs>
		auto cmp(const NState &ns, std::size_t gen, std::size_t nstk, XArgs... xargs) const {
			if (gen != ns.gen)
				return gen <=> ns.gen;
			if (boff != ns.boff)
				return ns.boff <=> boff;
			if (auto x = substack.cmp(ns.substack, nstk, xargs...); x != 0)
				return x;
			return 0 <=> 0;
		}
	};
	const Regexp &r;
	const minrx_regexec_flags_t flags;
	const bool minglobal = r.minglobal;
	const std::size_t suboff = r.nmin + r.nstk;
	const std::size_t minvcnt = (r.nmin + SizeBits - 1) / SizeBits;
	const std::size_t minvoff = suboff + 2 * r.nsub;
	const std::size_t nestoff = minvoff + minvcnt;
	std::size_t gen = 0;
	std::size_t off = 0;
	WConv wconv;
	WChar wcprev = WConv::End;
	Vec::Allocator allocator { nestoff + (minglobal ? r.nmin : 0) };
	std::optional<Vec> best;
	NInt bestmincount = 0; // note mincounts are negated so this means +infinity
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
				else if (auto x = ns.cmp(newns, gen, nstk, xargs...); x > 0 || (x == 0 && minvcnt && ns.substack.cmp(minvoff, minvcnt, newns.substack) > 0))
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
			else if (auto x = ns.cmp(newns, gen, nstk, xargs...); x > 0 || (x == 0 && minvcnt && ns.substack.cmp(minvoff, minvcnt, newns.substack) > 0))
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
			if (best.has_value() && ns.boff > best->get(suboff + 0))
				continue;
			const auto &n = nodes[k];
			auto nstk = n.nstk;
			switch (n.type) {
			case Node::Exit:
				{
					auto b = ns.boff, e = off, mincount = r.nmin ? ns.substack.get(0) : (NInt) -1;
					bool minvalid = r.nmin ? ns.substack.get(minvoff) < ((std::size_t) 1 << (SizeBits - 1)) : false;
					if (!best.has_value()
					    || b < best->get(suboff + 0)
					    || (b == best->get(suboff + 0) && e > best->get(suboff + 1) && (!minvalid || mincount >= bestmincount)))
					{
						best = ns.substack;
						best->put(suboff + 0, b);
						best->put(suboff + 1, e);
						if (minvalid)
							bestmincount = std::max(bestmincount, mincount);
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
				add(ncsv, k + 1, nstk, ns, wcnext, (NInt) off, (NInt) -1, (NInt) off);
				if (n.args[1])
					add(ncsv, k + 1 + n.args[0], nstk, ns, wcnext, (NInt) off, (NInt) 0, (NInt) off);
				break;
			case Node::MinB:
				{
					std::size_t w = n.args[0] / SizeBits;
					std::size_t b = (std::size_t) 1 << (SizeBits - 1 - n.args[0] % SizeBits);
					NState nscopy = ns;
					if (minglobal)
						b |= -b;
					auto x = nscopy.substack.get(minvoff + w);
					do {
						if ((x & b) != 0)
							nscopy.substack.put(minvoff + w, x & ~b);
						b = -1;
					} while (minglobal && w-- > 0);
					add(ncsv, k + 1, nstk, nscopy, wcnext);
				}
				break;
			case Node::MinL:
				add(ncsv, k + 1, nstk, ns, wcnext, off);
				break;
			case Node::MinR:
				{
					NState nscopy = ns;
					auto mininc = off - nscopy.substack.get(n.nstk);
					std::size_t oldlen = (std::size_t) -1 - nscopy.substack.get(n.args[0]);
					if (minglobal) {
						mininc -= nscopy.substack.get(nestoff + n.args[0]);
						nscopy.substack.put(nestoff + n.args[0], 0);
						nscopy.substack.put(n.args[0], (std::size_t) -1 - (oldlen + mininc));
						for (auto i = n.args[0]; i-- > 0; ) {
							oldlen = (std::size_t) -1 - nscopy.substack.get(i);
							nscopy.substack.put(i, -1 - (oldlen + mininc));
							nscopy.substack.put(nestoff + i, nscopy.substack.get(nestoff + i) + mininc);
						}
					} else {
						nscopy.substack.put(n.args[0], (std::size_t) -1 - (oldlen + mininc));
					}
					add(ncsv, k + 1, nstk, nscopy, wcnext);
				}
				break;
			case Node::MinX:
				{
					NState nscopy = ns;
					nscopy.substack.put(n.args[0], -1);
					std::size_t w = n.args[0] / SizeBits;
					std::size_t b = (std::size_t) 1 << (SizeBits - 1 - n.args[0] % SizeBits);
					nscopy.substack.put(minvoff + w, ns.substack.get(minvoff + w) | b);
					add(ncsv, k + 1, nstk, nscopy, wcnext);
				}
				break;
			case Node::Next:
				add(ncsv, k + 1, nstk, ns, wcnext);
				if (n.args[1] && off > ns.substack.get(nstk + 3 - 1))
					add(ncsv, k - n.args[0], nstk + 3, ns, wcnext, ns.substack.get(nstk), ns.substack.get(nstk + 1) - 1, (NInt) off);
				break;
			case Node::Skip:
				add(ncsv, k + 1, nstk, ns, wcnext, (NInt) off, (NInt) 1 ^ n.args[1]);
				add(ncsv, k + 1 + n.args[0], nstk, ns, wcnext, (NInt) off, (NInt) 0 ^ n.args[1]);
				break;
			case Node::SubL:
				{
					NState nscopy = ns;
					nscopy.substack.put(nstk - 1, off);
					if (n.args[0] != (NInt) -1 && (flags & MINRX_REG_NOSUBRESET) == 0)
						for (auto i = n.args[0] + 1; i <= n.args[1]; ++i) {
							nscopy.substack.put(suboff + i * 2, -1);
							nscopy.substack.put(suboff + i * 2 + 1, -1);
						}
					add(ncsv, k + 1, nstk, nscopy, wcnext);
				}
				break;
			case Node::SubR:
				if (n.args[0] != (NInt) -1 && ((flags & MINRX_REG_FIRSTSUB) == 0 || ns.substack.get(suboff + n.args[0] * 2) == (NInt) -1)) {
					NState nscopy = ns;
					nscopy.substack.put(suboff + n.args[0] * 2 + 0, ns.substack.get(nstk));
					nscopy.substack.put(suboff + n.args[0] * 2 + 1, off);
					add(ncsv, k + 1, nstk, nscopy, wcnext);
				} else {
					add(ncsv, k + 1, nstk, ns, wcnext);
				}
				break;
			case Node::ZBOB:
				if (off == 0 && (flags & MINRX_REG_NOTBOL) == 0)
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZEOB:
				if (wcnext == WConv::End && (flags & MINRX_REG_NOTEOL) == 0)
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZBOL:
				if (((off == 0 && (flags & MINRX_REG_NOTBOL) == 0)) || wcprev == L'\n')
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZEOL:
				if (((wcnext == WConv::End && (flags & MINRX_REG_NOTEOL) == 0)) || wcnext == L'\n')
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZBOW:
				if ((off == 0 || !is_word(wcprev)) && (wcnext != WConv::End && is_word(wcnext)))
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZEOW:
				if ((off != 0 && is_word(wcprev)) && (wcnext == WConv::End || !is_word(wcnext)))
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZXOW:
				if (   ((off == 0 || !is_word(wcprev)) && (wcnext != WConv::End && is_word(wcnext)))
				    || ((off != 0 && is_word(wcprev)) && (wcnext == WConv::End || !is_word(wcnext))))
					add(ncsv, k + 1, nstk, ns, wcnext);
				break;
			case Node::ZNWB:
				if (   (off == 0 && wcnext == WConv::End)
				    || (off == 0 && wcnext != WConv::End && !is_word(wcnext))
				    || (off != 0 && !is_word(wcprev) && wcnext == WConv::End)
				    || (off != 0 && wcnext != WConv::End && is_word(wcprev) == is_word(wcnext)))
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
		off = wconv.off();
		auto wcnext = wconv.nextchr();
		if ((flags & MINRX_REG_RESUME) != 0 && rm && rm[0].rm_eo > 0)
			while (wcnext != WConv::End && (std::ptrdiff_t) off < rm[0].rm_eo)
				wcprev = wcnext, off = wconv.off(), wcnext = wconv.nextchr();
		NState nsinit(allocator);
		nsinit.boff = off;
		if (minglobal)
			for (std::size_t i = 0; i < r.nmin; ++i)
				nsinit.substack.put(nestoff + i, 0);
		add(mcsvs[0], 0, 0, nsinit, wcnext);
		if (!epsq.empty())
			epsclosure(mcsvs[0], wcnext);
		for (;;) { // unrolled to ping-pong roles of mcsvs[0]/[1]
			if (wcnext == WConv::End)
				break;
			++gen;
			wcprev = wcnext, off = wconv.off(), wcnext = wconv.nextchr();
			while (!mcsvs[0].empty()) {
				auto [n, ns] = mcsvs[0].remove();
				add(mcsvs[1], n + 1, nodes[n].nstk, ns, wcnext);
			}
			if (!best.has_value()) {
				nsinit.boff = off;
				add(mcsvs[1], 0, 0, nsinit, wcnext);
			}
			if (!epsq.empty())
				epsclosure(mcsvs[1], wcnext);
			if (best.has_value() && mcsvs[1].empty())
				break;
			if (wcnext == WConv::End)
				break;
			++gen;
			wcprev = wcnext, off = wconv.off(), wcnext = wconv.nextchr();
			while (!mcsvs[1].empty()) {
				auto [n, ns] = mcsvs[1].remove();
				add(mcsvs[0], n + 1, nodes[n].nstk, ns, wcnext);
			}
			if (!best.has_value()) {
				nsinit.boff = off;
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
					rm[i].rm_so = (*best->storage)[suboff + i * 2];
					rm[i].rm_eo = (*best->storage)[suboff + i * 2 + 1];
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
