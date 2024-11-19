//
// MinRX: a minimal matcher for POSIX Extended Regular Expressions.
// Copyright (C) 2023, 2024 Michael J. Haertel.
//
// This file is part of MinRX.
//
// MinRX is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
//
// MinRX is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
#include <limits>
#include <map>
#include <mutex>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <vector>
#define CHARSET 1
#ifdef CHARSET
#include <memory>
#include "charset.h"
#endif
#include "minrx.h"

namespace MinRX {

template <typename UINT> inline auto ctz(UINT x) { return __builtin_ctz(x); }
template <> inline auto ctz(unsigned long x) { return __builtin_ctzl(x); }
template <> inline auto ctz(unsigned long long x) { return __builtin_ctzll(x); }

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
	bool cmpgt(const COWVec &other, std::size_t limit) const {
		std::size_t i = 0;
		TYPE *xv = &(*storage)[0];
		TYPE *yv = &(*other.storage)[0];
		while (i < limit && xv[i] == yv[i])
			++i;
		if (i == limit)
			return false;
		return xv[i] > yv[i];
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
	COWVec &sub(std::size_t idx, TYPE val) {
		if (storage->refcnt > 1) {
			--storage->refcnt;
			storage = storage->clone();
			storage->refcnt = 1;
		}
		(*storage)[idx] -= val;
		return *this;
	}
};

template <typename UINT>
struct QSet {
	std::uint64_t *bits[10];
	unsigned int depth = 0;
	QSet(UINT limit) {
		std::size_t s[10], t = 0;
		do
			t += (limit = s[depth++] = (limit + 63u) / 64u);
		while (limit > 1);
		bits[0] = (std::uint64_t *) ::operator new(t * sizeof (std::uint64_t));
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
		for (unsigned int i = 1; i < depth; ++i)
			bits[i] = bits[i - 1] + s[i - 1];
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic pop
#endif
		bits[depth - 1][0] = 0;
	}
	~QSet() { ::operator delete(bits[0]); }
	bool contains(UINT k) const {
		for (auto d = depth; d > 0; ) {
			std::size_t idx = k >> 6 * d;
			unsigned int bit = (k >> 6 * --d) & 0x3f;
			if ((bits[d][idx] & ((std::uint64_t) 1 << bit)) == 0)
				return false;
		}
		return true;
	}
	bool empty() const { return !bits[depth - 1][0]; }
	UINT remove() {
		UINT k = 0;
		auto d = depth;
		do {
			auto m = bits[--d][k];
			k = (k << 6) | ctz(m);
		} while (d != 0);
		UINT r = k;
		for (; d < depth && !(bits[d][k >> 6] &= ~((std::uint64_t) 1 << (k & 0x3f))); k >>= 6, ++d)
			;
		return r;
	}
	bool insert(UINT k) {
		bool r = false;
		for (auto d = depth; d-- != 0; ) {
			auto bp = bits[d] + ((k >> 6 * d) >> 6);
			std::uint64_t m = (std::uint64_t) 1 << ((k >> 6 * d) & 0x3f);
			if ((*bp & m) == 0) {
				if (d)
					bits[d - 1][k >> (6 * d)] = 0;
				else
					r = true;
			}
			*bp |= m;
		}
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
		bool r = qset.insert(k);
		if (r)
			new (storage + k) DATA(v);
		return {r, storage[k]};
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
#ifdef CHARSET
	friend class CSet;
#endif
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
	CSet() {
		int errcode = 0;
		charset = charset_create(& errcode);
		// FIXME: Throw error if charset == nullptr
	}
	CSet(const CSet &) = delete;
	CSet &operator=(const CSet &) = delete;
	CSet(CSet &&cs): charset(cs.charset) { cs.charset = nullptr; }
	CSet &operator=(CSet &&cs) { charset = cs.charset; cs.charset = nullptr; return *this; }
	~CSet() { if (charset) { charset_free(charset); charset = nullptr; } }
	bool test(WChar wc) const { return charset_in_set(charset, wc); }
	CSet &invert() {
		charset_invert(charset); // FIXME: no error checking
		return *this;
	}
	minrx_result_t parse(minrx_regcomp_flags_t flags, WConv::Encoding enc, std::size_t mbmax, WConv &wconv) {
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
					int32_t coll[2] = { wc, L'\0' };
					charset_add_collate(charset, coll);	// FIXME: No error checking
					if ((flags & MINRX_REG_ICASE) != 0) {
						if (std::iswlower(wc))
							coll[0] = std::towupper(wc);
						else if (std::iswupper(wc))
							coll[0] = std::towlower(wc);
						charset_add_collate(charset, coll);	// FIXME: No error checking
					}
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
					charset_add_equiv(charset, wc);	// FIXME: No error checking
					if ((flags & MINRX_REG_ICASE) != 0) {
						if (std::iswlower(wc))
							charset_add_equiv(charset, std::towupper(wc));	// FIXME: no error checking
						else if (std::iswupper(wc))
							charset_add_equiv(charset, std::towlower(wc));	// FIXME: no error checking
					}
					wc = wconv.nextchr().look();
					if (wc != L'.' || (wc = wconv.nextchr().look() != L']'))
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
					if (std::iswlower(wclo) && std::iswlower(wchi)) {
						set(std::towupper(wclo), std::towupper(wchi));
					} else if (std::iswupper(wclo) && std::iswupper(wchi)) {
						set(std::towlower(wclo), std::towlower(wchi));
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
	CSet &set(WChar wclo, WChar wchi) {
		charset_add_range(charset, wclo, wchi);	// FIXME: no error checking
		return *this;
	}
	CSet &set(WChar wc) {
		charset_add_char(charset, wc);	// FIXME: no error checking
		return *this;
	}
	bool cclass(minrx_regcomp_flags_t flags, WConv::Encoding /*enc*/, const std::string &name) {
		int result = charset_add_cclass(charset, name.c_str());
		if ((flags & MINRX_REG_ICASE) != 0) {
			if (name == "lower")
				charset_add_cclass(charset, "upper");	// FIXME: Add error checking
			else if (name == "upper")
				charset_add_cclass(charset, "lower");	// FIXME: Add error checking
		}
		return result == CSET_SUCCESS;
	}
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
	bool inverted = false;
	CSet &operator|=(const CSet &cs) {
		for (const auto &e : cs.ranges)
			set(e.min, e.max);
		return *this;
	}
	CSet &invert() { inverted = true; return *this; }
	CSet &set(WChar wclo, WChar wchi) {
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
		return *this;
	}
	CSet &set(WChar wc) { return set(wc, wc); }
	bool test(WChar wc) const {
		if (wc < 0)
			return false;
		auto i = ranges.lower_bound(Range(wc, wc));
		return inverted ^ (i != ranges.end() && wc >= i->min && wc <= i->max);
	}
	bool cclass(minrx_regcomp_flags_t flags, WConv::Encoding enc, const std::string &name) {
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
	}
	minrx_result_t parse(minrx_regcomp_flags_t flags, WConv::Encoding enc, std::size_t /*mbmax*/, WConv &wconv) {
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
					// FIXME: recognize some equivalence classes.
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
				if ((flags & MINRX_REG_ICASE) != 0)
					for (auto wc = wclo; wc <= wchi; ++wc) {
						set(enc == WConv::Encoding::Byte ? std::tolower(wc) : std::towlower(wc));
						set(enc == WConv::Encoding::Byte ? std::toupper(wc) : std::towupper(wc));
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
#endif
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
		Fork,			// args = priority delta, offset to next
		Goto,			// args = offset to join, offset to next
		Join,			// args = offset to fork
		Loop,			// args = offset to next, optional flag
		Next,			// args = offset to loop, infinite flag
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
	std::size_t mbmax;
	WConv wconv;
	std::vector<CSet> csets;
	std::optional<std::size_t> dot;
	std::optional<std::size_t> esc_s;
	std::optional<std::size_t> esc_S;
	std::optional<std::size_t> esc_w;
	std::optional<std::size_t> esc_W;
	std::map<WChar, unsigned int> icmap;
	NInt nsub = 0;
	Compile(WConv::Encoding e, const char *bp, const char *ep, minrx_regcomp_flags_t flags, std::size_t mbmax): flags(flags), enc(e), mbmax(mbmax), wconv(e, bp, ep) { wconv.nextchr(); }
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
				l.nstk += 2;
			std::vector<Subexp> alts;
			while (wconv.look() == L'|') {
				wconv.nextchr();
				alts.push_back(cat(nested, nstk + 1));
			}
			auto [rhs, rhmaxstk, err] = alts.back();
			if (err)
				return {rhs, rhmaxstk, err};
			rhs.push_front({Node::Goto, {rhs.size(), rhs.size()}, nstk + 1});
			alts.pop_back();
			while (!alts.empty()) {
				auto [mhs, mhmaxstk, _] = alts.back();
				alts.pop_back();
				rhs.insert(rhs.begin(), mhs.begin(), mhs.end());
				rhmaxstk = std::max(mhmaxstk, rhmaxstk);
				rhs.push_front({Node::Goto, {rhs.size(), mhs.size()}, nstk + 1});
			}
			lhs.push_front({Node::Fork, {(NInt) -1, lhs.size()}, nstk});
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
			lhs.push_front({Node::Fork, {(NInt) 1, lhsize}, nstk});
			lhs.push_back({Node::Join, {lhsize, 0}, nstk + 1});
			return {lhs, lhmaxstk + 1, MINRX_REG_SUCCESS};
		} else {
			for (auto &l : lhs) l.nstk += 3;
			auto lhsize = lhs.size();
			lhs.push_front({Node::Loop, {lhsize, (NInt) optional}, nstk});
			lhs.push_back({Node::Next, {lhsize, (NInt) infinite}, nstk + 3});
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
			rhs.push_front({Node::Fork, {1, rhsize}, nstk});
			rhs.push_back({Node::Join, {rhsize, 0}, nstk + 1});
			for (; k < n; ++k)
				lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		}
		if (n == (NInt) -1) {
			lhmaxstk += 3;
			rhmaxstk += 3;
			for (auto &r : rhs)
				r.nstk += 3;
			auto rhsize = rhs.size();
			rhs.push_front({Node::Loop, {rhsize, 0}, nstk});
			rhs.push_back({Node::Next, {rhsize, 1}, nstk + 3});
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
						csets.emplace_back();
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
			if (auto err = csets.emplace_back().parse(flags, enc, mbmax, wconv))
				return {{}, 0, err};
			break;
		case L'.':
			lhmaxstk = nstk;
			if (!dot.has_value()) {
				dot = csets.size();
				csets.emplace_back();
				if ((flags & MINRX_REG_NEWLINE) != 0)
					csets.back().set(L'\n');
				csets.back().invert();
			}
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
					csets.emplace_back().parse(flags, enc, mbmax, wc.nextchr());
				}
				lhs.push_back({Node::CSet, {*esc_s, 0}, nstk});
				break;
			case L'S':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_S.has_value()) {
					esc_S = csets.size();
					WConv wc(enc, "[^[:space:]]");
					csets.emplace_back().parse(flags, enc, mbmax, wc.nextchr());
				}
				lhs.push_back({Node::CSet, {*esc_S, 0}, nstk});
				break;
			case L'w':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_w.has_value()) {
					esc_w = csets.size();
					WConv wc(enc, "[[:alnum:]_]");
					csets.emplace_back().parse(flags, enc, mbmax, wc.nextchr());
				}
				lhs.push_back({Node::CSet, {*esc_w, 0}, nstk});
				break;
			case L'W':
				if ((flags & MINRX_REG_EXTENSIONS_GNU) == 0)
					goto normal;
				if (!esc_W.has_value()) {
					esc_W = csets.size();
					WConv wc(enc, "[^[:alnum:]_]");
					csets.emplace_back().parse(flags, enc, mbmax, wc.nextchr());
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
				lhs.push_front({Node::SubL, {n, nsub}, nstk});
				lhs.push_back({Node::SubR, {n, nsub}, nstk + 1});
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
		std::size_t boff;
		Vec substack;
		NState() {}
		NState(Vec::Allocator &allocator): substack(allocator) {}
		bool cmpgt(const NState &ns, std::size_t nstk) const {
			return boff != ns.boff ? boff < ns.boff : substack.cmpgt(ns.substack, nstk);
		}
	};
	const Regexp &r;
	const minrx_regexec_flags_t flags;
	WConv wconv;
	WChar lookback = WConv::End;
	Vec::Allocator allocator { r.nstk + 2 * r.nsub };
	std::optional<COWVec<std::size_t, (std::size_t) -1>> best;
	QSet<NInt> epsq { r.nodes.size() };
	QVec<NInt, NState> epsv { r.nodes.size() };
	Execute(const Regexp &r, minrx_regexec_flags_t flags, const char *bp, const char *ep) : r(r), flags(flags), wconv(r.enc, bp, ep) {}
	void add(QVec<NInt, NState> &ncsv, NInt n, const NState &ns) {
		if (r.nodes[n].type <= Node::CSet) {
			auto [newly, oldns] = ncsv.insert(n, ns);
			if (!newly && ns.cmpgt(oldns, r.nodes[n].nstk))
				oldns = ns;
		} else {
			auto [newly, oldns] = epsv.insert(n, ns);
			if (newly || (ns.cmpgt(oldns, r.nodes[n].nstk) && (oldns = ns, true)))
				epsq.insert(n);
		}
	}
	void epsclosure(QVec<NInt, NState> &ncsv) {
		auto nodes = r.nodes;
		auto is_word = r.enc == WConv::Encoding::Byte ? [](WChar b) { return b == '_' || std::isalnum(b); }
							      : [](WChar wc) { return wc == L'_' || std::iswalnum(wc); };
		do {
			NInt k = epsq.remove();
			NState &ns = epsv.lookup(k);
			if (best.has_value() && ns.boff > best->get(r.nstk + 0))
				continue;
			const auto &n = nodes[k];
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
					NState nscopy = ns;
					NInt pridelta = n.args[0];
					NInt priority = 0;
					do {
						nscopy.substack.put(n.nstk, priority += pridelta);
						add(ncsv, k + 1, nscopy);
						k = k + 1 + nodes[k].args[1];
					} while (nodes[k].type != Node::Join);
					if (priority == pridelta) {
						nscopy.substack.put(n.nstk, priority += pridelta);
						add(ncsv, k, nscopy);
					}
				}
				break;
			case Node::Goto:
				add(ncsv, k + 1 + n.args[1], ns);
				break;
			case Node::Join:
				add(ncsv, k + 1, ns);
				break;
			case Node::Loop:
				{
					NState nscopy = ns;
					nscopy.substack.put(n.nstk, wconv.off());
					nscopy.substack.put(n.nstk + 1, -1);
					nscopy.substack.put(n.nstk + 2, wconv.off());
					add(ncsv, k + 1, nscopy);
					if (n.args[1]) {
						nscopy.substack.put(n.nstk + 1, 0);
						add(ncsv, k + 1 + n.args[0], nscopy);
					}
				}
				break;
			case Node::Next:
				{
					add(ncsv, k + 1, ns);
					if (n.args[1] && wconv.off() > ns.substack.get(n.nstk - 1)) {
						NState nscopy = ns;
						nscopy.substack.sub(n.nstk - 2, 1);
						nscopy.substack.put(n.nstk - 1, wconv.off());
						add(ncsv, k - n.args[0], nscopy);
					}
				}
				break;
			case Node::SubL:
				{
					NState nscopy = ns;
					nscopy.substack.put(n.nstk, wconv.off());
					if (n.args[0] != (NInt) -1)
						for (auto i = n.args[0]; i <= n.args[1]; ++i) {
							nscopy.substack.put(r.nstk + i * 2, -1);
							nscopy.substack.put(r.nstk + i * 2 + 1, -1);
						}
					add(ncsv, k + 1, nscopy);
				}
				break;
			case Node::SubR:
				if (n.args[0] != (NInt) -1) {
					NState nscopy = ns;
					nscopy.substack.put(r.nstk + n.args[0] * 2 + 0, ns.substack.get(n.nstk - 1));
					nscopy.substack.put(r.nstk + n.args[0] * 2 + 1, wconv.off());
					add(ncsv, k + 1, nscopy);
				} else {
					add(ncsv, k + 1, ns);
				}
				break;
			case Node::ZBOB:
				if (wconv.off() == 0 && (flags & MINRX_REG_NOTBOL) == 0)
					add(ncsv, k + 1, ns);
				break;
			case Node::ZEOB:
				if (wconv.look() == WConv::End && (flags & MINRX_REG_NOTEOL) == 0)
					add(ncsv, k + 1, ns);
				break;
			case Node::ZBOL:
				if (((wconv.off() == 0 && (flags & MINRX_REG_NOTBOL) == 0)) || lookback == L'\n')
					add(ncsv, k + 1, ns);
				break;
			case Node::ZEOL:
				if (((wconv.look() == WConv::End && (flags & MINRX_REG_NOTEOL) == 0)) || wconv.look() == L'\n')
					add(ncsv, k + 1, ns);
				break;
			case Node::ZBOW:
				if ((wconv.off() == 0 || !is_word(lookback)) && (wconv.look() != WConv::End && is_word(wconv.look())))
					add(ncsv, k + 1, ns);
				break;
			case Node::ZEOW:
				if ((wconv.off() != 0 && is_word(lookback)) && (wconv.look() == WConv::End || !is_word(wconv.look())))
					add(ncsv, k + 1, ns);
				break;
			case Node::ZXOW:
				if (   ((wconv.off() == 0 || !is_word(lookback)) && (wconv.look() != WConv::End && is_word(wconv.look())))
				    || ((wconv.off() != 0 && is_word(lookback)) && (wconv.look() == WConv::End || !is_word(wconv.look()))))
					add(ncsv, k + 1, ns);
				break;
			case Node::ZNWB:
				if (   (wconv.off() == 0 && wconv.look() == WConv::End)
				    || (wconv.off() == 0 && wconv.look() != WConv::End && !is_word(wconv.look()))
				    || (wconv.off() != 0 && !is_word(lookback) && wconv.look() == WConv::End)
				    || (wconv.off() != 0 && wconv.look() != WConv::End && is_word(lookback) == is_word(wconv.look())))
					add(ncsv, k + 1, ns);
				break;
			default:
				abort();
				break;
			}
		} while (!epsq.empty());
	}
	int execute(std::size_t nm, minrx_regmatch_t *rm) {
		QVec<NInt, NState> mcsvs[2] { r.nodes.size(), r.nodes.size() };
		auto nodes = &r.nodes[0];
		wconv.nextchr();
		if ((flags & MINRX_REG_RESUME) != 0 && rm && rm[0].rm_eo > 0)
			while (wconv.look() != WConv::End && (std::ptrdiff_t) wconv.off() < rm[0].rm_eo)
				lookback = wconv.look(), wconv.nextchr();
		NState nsinit(allocator);
		nsinit.boff = wconv.off();
		add(mcsvs[0], 0, nsinit);
		if (!epsq.empty())
			epsclosure(mcsvs[0]);
		auto wc = wconv.look();
		for (;;) { // unrolled to ping-pong roles of mcsvs[0]/[1]
			if (wc == WConv::End)
				break;
			epsv.clear();
			while (!mcsvs[0].empty()) {
				auto [n, ns] = mcsvs[0].remove();
				auto t = nodes[n].type;
				if (t <= WCharMax) {
					if (wc != (WChar) t)
						continue;
				} else {
					if (!r.csets[nodes[n].args[0]].test(wc))
						continue;
				}
				add(mcsvs[1], n + 1, ns);
			}
			wconv.nextchr(), lookback = wc, wc = wconv.look();
			if (!best.has_value()) {
				nsinit.boff = wconv.off();
				add(mcsvs[1], 0, nsinit);
			}
			if (!epsq.empty())
				epsclosure(mcsvs[1]);
			if (best.has_value() && mcsvs[1].empty())
				break;
			if (wc == WConv::End)
				break;
			epsv.clear();
			while (!mcsvs[1].empty()) {
				auto [n, ns] = mcsvs[1].remove();
				auto t = nodes[n].type;
				if (t <= WCharMax) {
					if (wc != (WChar) t)
						continue;
				} else {
					if (!r.csets[nodes[n].args[0]].test(wc))
						continue;
				}
				add(mcsvs[0], n + 1, ns);
			}
			wconv.nextchr(), lookback = wc, wc = wconv.look();
			if (!best.has_value()) {
				nsinit.boff = wconv.off();
				add(mcsvs[0], 0, nsinit);
			}
			if (!epsq.empty())
				epsclosure(mcsvs[0]);
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
	auto mbmax = MB_CUR_MAX;
	if ((loc != nullptr && loc[0] == 'C' && loc[1] == '\0') || ((flags & MINRX_REG_NATIVE1B) != 0 && mbmax == 1))
		enc = MinRX::WConv::Encoding::Byte;
	else if (auto utf = std::strchr(loc ? loc : "", '.');
		 utf != nullptr && (utf[1] == 'U' || utf[1] == 'u')
				&& (utf[2] == 'T' || utf[2] == 't')
				&& (utf[3] == 'F' || utf[3] == 'f')
				&& (   (utf[4] == '8' && utf[5] == '\0')
				    || (utf[4] == '-' && utf[5] == '8' && utf[6] == '\0')))
		enc = MinRX::WConv::Encoding::UTF8;
	auto r = MinRX::Compile(enc, s, s + ns, (minrx_regcomp_flags_t) flags, mbmax).compile();
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
		"success",
		"bad pattern",
		"invalid contents of {}",
		"? * + or {interval} not preceded by valid subpattern",
		"unbalanced {",
		"unbalanced [",
		"invalid collating element",
		"invalid character class name",
		"invalid trailing backslash",
		"unbalanced (",
		"invalid range endpoint",
		"memory allocation failed",
		"invalid \\digit",
		"match not found",
		"unknown error code",
	};
	if (errcode < 0 || errcode > MINRX_REG_UNKNOWN)
		errcode = MINRX_REG_UNKNOWN;
	size_t size = snprintf(errbuf, errsize, "%s", messages[errcode]);
	if (errsize != 0 && size == errsize)
		errbuf[errsize - 1] = '\0';
	return size + 1;
}
