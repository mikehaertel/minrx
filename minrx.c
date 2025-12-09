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

// ISO C
#include <ctype.h>
#include <limits.h>
#include <locale.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>

// POSIX
#include <langinfo.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

// GNU gettext
#ifdef HAVE_GETTEXT_H
#include <gettext.h>
#define _(msgid)  gettext(msgid)
#else /* ! HAVE_GETTEXT_H */
#define _(msgid)  msgid
#endif /* ! HAVE_GETTEXT_H */
#define N_(msgid) msgid

#include "minrx.h"

// Arnold Robbins' charset library
#include "charset.c"

// Hack to reconcile C compound literal syntax with C++ initializer lists
#ifdef __cplusplus
#define LITERAL(TYPE)
#else
#define LITERAL(TYPE) (TYPE)
#endif

#ifdef __GNUC__
#define inline __attribute__((__always_inline__)) inline
#endif

static inline
bool IsDigit(int32_t wc)
{
	return wc >= L'0' && wc <= L'9';
}

static int
cmp(size_t x, size_t y)
{
	return (x > y) - (x < y);
}

#ifdef __GNUC__
// FIXME: expand compiler support beyond clang and gcc
#if UINT_MAX == 18446744073709551615U
#define ctz __builtin_ctz
#elif ULONG_MAX == 18446744073709551615U
#define ctz __builtin_ctzl
#elif ULLONG_MAX == 18446744073709551615U
#define ctz __builtin_ctzll
#else
#error "can't figure out how to count trailing zeroes for 64-bit operands"
#endif
#else
// ctz --- count trailing zeros. dead simple table driven version

static int
ctz(uint64_t val)
{
	static char table[] = {
		8, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		7, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
		4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
	};

	if (val == 0)
		return 64;

	int count = 0;
	for (; val != 0; val >>= 8) {
		uint32_t x = val & 0xFF;

		if (x == 0) {
			count += 8;
			continue;
		}

		count += table[x];
		break;
	}

	return count;
}
#endif

#define MAX(A, B) ((A) >= (B) ? (A) : (B))
#define MIN(A, B) ((A) <= (B) ? (A) : (B))

typedef struct COWVec_Allocator COWVec_Allocator;
typedef struct COWVec_Storage COWVec_Storage;
typedef struct COWVec COWVec;

struct COWVec_Allocator {
	size_t length;
	COWVec_Storage *freelist;
};

struct COWVec_Storage {
	union {
		COWVec_Allocator *allocator;
		COWVec_Storage *freelink;
	} u;
	size_t refcnt;
	size_t hack[1];
};

struct COWVec {
	COWVec_Storage *storage;
};

static void
cowvec_allocator_construct(COWVec_Allocator *a, size_t length)
{
	a->length = length;
	a->freelist = (COWVec_Storage *) NULL;
}

static void
cowvec_allocator_destruct(COWVec_Allocator *a)
{
	for (COWVec_Storage *s = a->freelist, *sfreelink = (COWVec_Storage *) NULL; s != (COWVec_Storage *) NULL; s = sfreelink) {
		sfreelink = s->u.freelink;
		free(s);
	}
}

static size_t
cowvec_storage_get(const COWVec_Storage *cvs, size_t i)
{
	return (&cvs->hack[0])[i];
}

static const size_t *
cowvec_storage_getref(const COWVec_Storage *cvs, size_t i)
{
	return &(&cvs->hack[0])[i];
}

static void
cowvec_storage_put(COWVec_Storage *cvs, size_t i, size_t v)
{
	(&cvs->hack[0])[i] = v;
}

static COWVec_Storage *
cowvec_storage_alloc(COWVec_Allocator *a)
{
	COWVec_Storage *r;
	if (a->freelist) {
		r = a->freelist;
		a->freelist = r->u.freelink;
		r->u.allocator = a;
		r->refcnt = 1;
	} else {
		r = (COWVec_Storage *) malloc(sizeof (COWVec_Storage) + (a->length - 1) * sizeof (size_t));
		r->u.allocator = a;
		r->refcnt = 1;
	}
	for (size_t i = 0, n = a->length; i != n; ++i)
		cowvec_storage_put(r, i, -1);
	return r;
}

static COWVec_Storage *
cowvec_storage_clone(COWVec_Storage *cvs)
{
	COWVec_Storage *newcvs = cowvec_storage_alloc(cvs->u.allocator);
	for (size_t i = 0, n = cvs->u.allocator->length; i != n; ++i)
		cowvec_storage_put(newcvs, i, cowvec_storage_get(cvs, i));
	return newcvs;
}

static void
cowvec_storage_dealloc(COWVec_Allocator *a, COWVec_Storage *s)
{
	s->u.freelink = a->freelist;
	a->freelist = s;
}

static void
cowvec_construct(COWVec *c, COWVec_Allocator *a)
{
	c->storage = a ? cowvec_storage_alloc(a) : (COWVec_Storage *) NULL;
}

static void
cowvec_destruct(COWVec *c)
{
	if (c->storage && --c->storage->refcnt == 0)
		cowvec_storage_dealloc(c->storage->u.allocator, c->storage);
	c->storage = (COWVec_Storage *) NULL;
}

static size_t
cowvec_get(COWVec *cv, size_t idx)
{
	return cowvec_storage_get(cv->storage, idx);
}

static void
cowvec_put(COWVec *cv, size_t idx, size_t val)
{
	if (cowvec_storage_get(cv->storage, idx) == val)
		return;
	if (cv->storage->refcnt > 1) {
		--cv->storage->refcnt;
		cv->storage = cowvec_storage_clone(cv->storage);
		cv->storage->refcnt = 1;
	}
	cowvec_storage_put(cv->storage, idx, val);
}

static void
cowvec_copy(COWVec *dst, const COWVec *src)
{
	++src->storage->refcnt;
	if (dst->storage && --dst->storage->refcnt == 0)
		cowvec_storage_dealloc(dst->storage->u.allocator, dst->storage);
	dst->storage = src->storage;
}

static void
cowvec_move(COWVec *dst, COWVec *src)
{
	if (dst->storage && --dst->storage->refcnt == 0)
		cowvec_storage_dealloc(dst->storage->u.allocator, dst->storage);
	dst->storage = src->storage;
	src->storage = (COWVec_Storage *) NULL;
}

static int
cowvec_cmp(const COWVec *xcv, const COWVec *ycv, size_t limit)
{
	size_t i;
	const size_t *xv = cowvec_storage_getref(xcv->storage, 0);
	const size_t *yv = cowvec_storage_getref(ycv->storage, 0);
	for (i = 0; i < limit; i++)
		if (xv[i] != yv[i])
			return cmp(xv[i], yv[i]);
	return 0;
}

static int
cowvec_cmp_1(const COWVec *xcv, const COWVec *ycv, size_t limit, size_t arg1)
{
	size_t i;
	const size_t *xv = cowvec_storage_getref(xcv->storage, 0);
	const size_t *yv = cowvec_storage_getref(ycv->storage, 0);
	for (i = 0; i < limit - 1; i++)
		if (xv[i] != yv[i])
			return cmp(xv[i], yv[i]);
	if (arg1 != yv[limit - 1])
		return cmp(arg1, yv[limit - 1]);
	return 0;
}

static int
cowvec_cmp_2(const COWVec *xcv, const COWVec *ycv, size_t limit, size_t arg1, size_t arg2)
{
	size_t i;
	const size_t *xv = cowvec_storage_getref(xcv->storage, 0);
	const size_t *yv = cowvec_storage_getref(ycv->storage, 0);
	for (i = 0; i < limit - 2; i++)
		if (xv[i] != yv[i])
			return cmp(xv[i], yv[i]);
	if (arg1 != yv[limit - 2])
		return cmp(arg1, yv[limit - 2]);
	if (arg2 != yv[limit - 1])
		return cmp(arg2, yv[limit - 1]);
	return 0;
}

static int
cowvec_cmp_3(const COWVec *xcv, const COWVec *ycv, size_t limit, size_t arg1, size_t arg2, size_t arg3)
{
	size_t i;
	const size_t *xv = cowvec_storage_getref(xcv->storage, 0);
	const size_t *yv = cowvec_storage_getref(ycv->storage, 0);
	for (i = 0; i < limit - 3; i++)
		if (xv[i] != yv[i])
			return cmp(xv[i], yv[i]);
	if (arg1 != yv[limit - 3])
		return cmp(arg1, yv[limit - 3]);
	if (arg2 != yv[limit - 2])
		return cmp(arg2, yv[limit - 2]);
	if (arg3 != yv[limit - 1])
		return cmp(arg3, yv[limit - 1]);
	return 0;
}

static int
cowvec_cmp_from(size_t o, const COWVec *xcv, const COWVec *ycv, size_t n)
{
	const size_t *xv = cowvec_storage_getref(xcv->storage, 0);
	const size_t *yv = cowvec_storage_getref(ycv->storage, 0);
	for (size_t i = 0; i < n; i++)
		if (xv[o + i] != yv[o + i])
			return cmp(xv[o + i], yv[o + i]);
	return 0;
}

static bool
cowvec_valid(const COWVec *c)
{
	return c->storage != (COWVec_Storage *) NULL;
}

typedef struct QSet QSet;
struct QSet {
	uint64_t *bits[10];
	uint64_t bits0;
	uint64_t *bitsfree;
	int depth;
};

static bool
qset_construct(QSet *q, size_t limit)
{
	size_t s[10], t = 0;
	q->depth = 0;
	do
		t += (limit = s[q->depth++] = (limit + 63u) / 64u);
	while (limit > 1);
	uint64_t *next = q->bitsfree = (uint64_t *) malloc(t * sizeof (uint64_t));
	if (!next)
		return false;
	q->bits[0] = &q->bits0;
	for (int i = 1; i < q->depth; ++i)
		q->bits[i] = next, next += s[q->depth - 1 - i];
	q->bits0 = 0;
	return true;
}

static void
qset_destruct(QSet *q)
{
	if (q->bitsfree)
		free(q->bitsfree);
}

static uint64_t
bit(size_t k)
{
	return (uint64_t) 1 << (k & 0x3F);
}

static bool
qset_empty(const QSet *q)
{
	return q->bits0 == 0;
}

static bool
qset_contains(const QSet *q, size_t k)
{
	int i = 0, s = 6 * q->depth;
	size_t j = 0;
	while (i < q->depth) {
		int64_t x = q->bits[i++][j];
		s -= 6;
		j = k >> s;
		int64_t w = bit(j);
		if (!(x & w))
			return false;
	}
	return true;
}

static bool
qset_insert(QSet *q, size_t k)
{
	bool r = false;
	int i = 0, s = 6 * q->depth;
	size_t j = 0;
	while (i < q->depth) {
		uint64_t *bp = &q->bits[i++][j];
		uint64_t x = *bp;
		s -= 6;
		j = k >> s;
		uint64_t w = bit(j);
		if ((x & w) == 0) {
			if (i < q->depth)
				q->bits[i][j] = 0;
			else
				r = true;
		}
		*bp = x | w;
	}
	return r;
}

static size_t
qset_remove(QSet *q) // caller must ensure !empty()
{
	size_t k = 0;
	int i = 0, d = q->depth;
	do
		k = (k << 6) | ctz(q->bits[i++][k]);
	while (i != d);
	size_t r = k;
	do {
		--i;
		uint64_t w = bit(k);
		k >>= 6;
		if ((q->bits[i][k] &= ~w) != 0)
			break;
	} while (i != 0);
	return r;
}

typedef size_t NInt;

typedef struct NState NState;
struct NState {
	size_t gen;
	size_t boff;
	COWVec substack;
};

static void
nstate_copy(NState *dstns, const NState *srcns)
{
	dstns->gen = srcns->gen;
	dstns->boff = srcns->boff;
	cowvec_copy(&dstns->substack, &srcns->substack);
}

static void
nstate_move(NState *dstns, NState *srcns)
{
	dstns->gen = srcns->gen;
	dstns->boff = srcns->boff;
	cowvec_move(&dstns->substack, &srcns->substack);
}

static void
nstate_construct(NState *ns, COWVec_Allocator *a)
{
	ns->gen = 0;
	ns->boff = 0;
	cowvec_construct(&ns->substack, a);
}

static void
nstate_destruct(NState *ns)
{
	cowvec_destruct(&ns->substack);
}

static void
nstate_construct_copy(NState *dstns, const NState *srcns)
{
	nstate_construct(dstns, (COWVec_Allocator *) NULL);
	nstate_copy(dstns, srcns);
}

static void
nstate_construct_move(NState *dstns, NState *srcns)
{
	nstate_construct(dstns, (COWVec_Allocator *) NULL);
	nstate_move(dstns, srcns);
}

static int
nstate_cmp(const NState *xns, const NState *yns, size_t gen, size_t nstk)
{
	if (gen != yns->gen)
		return cmp(gen, yns->gen);
	if (xns->boff != yns->boff)
		return cmp(yns->boff, xns->boff);
	int x = cowvec_cmp(&xns->substack, &yns->substack, nstk);
	if (x != 0)
		return x;
	return 0;
}

static int
nstate_cmp_1(const NState *xns, const NState *yns, size_t gen, size_t nstk, NInt arg1)
{
	if (gen != yns->gen)
		return cmp(gen, yns->gen);
	if (xns->boff != yns->boff)
		return cmp(yns->boff, xns->boff);
	int x = cowvec_cmp_1(&xns->substack, &yns->substack, nstk, arg1);
	if (x != 0)
		return x;
	return 0;
}

static int
nstate_cmp_2(const NState *xns, const NState *yns, size_t gen, size_t nstk, NInt arg1, NInt arg2)
{
	if (gen != yns->gen)
		return cmp(gen, yns->gen);
	if (xns->boff != yns->boff)
		return cmp(yns->boff, xns->boff);
	int x = cowvec_cmp_2(&xns->substack, &yns->substack, nstk, arg1, arg2);
	if (x != 0)
		return x;
	return 0;
}

static int
nstate_cmp_3(const NState *xns, const NState *yns, size_t gen, size_t nstk, NInt arg1, NInt arg2, NInt arg3)
{
	if (gen != yns->gen)
		return cmp(gen, yns->gen);
	if (xns->boff != yns->boff)
		return cmp(yns->boff, xns->boff);
	int x = cowvec_cmp_3(&xns->substack, &yns->substack, nstk, arg1, arg2, arg3);
	if (x != 0)
		return x;
	return 0;
}

typedef struct QVec QVec;
struct QVec {
	QSet qset;
	NState *storage;
};

static bool
qvec_construct(QVec *q, size_t l)
{
	if (!qset_construct(&q->qset, l))
		return false;
	q->storage = (NState *) NULL;
	q->storage = (NState *) malloc(l * sizeof (NState));
	return true;
}

static void
qvec_clear(QVec *q)
{
	while (!qset_empty(&q->qset)) {
		size_t i = qset_remove(&q->qset);
		nstate_destruct(&q->storage[i]);
	}
}

static void
qvec_destruct(QVec *q)
{
	qvec_clear(q);
	free((void *) q->storage);
	qset_destruct(&q->qset);
}

static bool
qvec_empty(QVec *q)
{
	return qset_empty(&q->qset);
}

typedef struct QVecInsert QVecInsert;
struct QVecInsert {
	bool newly;
	NState *newnsp;
};

static QVecInsert
qvec_insert(QVec *q, size_t k, const NState *unused)
{
	bool newly = qset_insert(&q->qset, k);
	// WARNING: if newly inserted then we are returning a reference to uninitialized memory
	// and it is the caller's responsibility to construct a valid NState there.
	return LITERAL(QVecInsert) {newly, &q->storage[k]};
}

static NState *
qvec_lookup(QVec *q, size_t k)
{
	return &q->storage[k];
}

typedef struct QVecRemove QVecRemove;
struct QVecRemove {
	size_t index;
	NState nstate;
};

static QVecRemove
qvec_remove(QVec *q)
{
	size_t k = qset_remove(&q->qset);
	QVecRemove r;
	r.index = k;
	nstate_construct_move(&r.nstate, &q->storage[k]);
	return r;
}

typedef int32_t WChar;			// because wchar_t may not be 32 bits
#define WCharMax ((WChar) 0x10FFFF)	/* maximum code point: valid for Unicode and (FIXME!) blithely assumed for non-Unicode */
typedef enum WConv_Encoding { Byte, MBtoWC, UTF8 } WConv_Encoding;
enum { End = -1 };

typedef struct WConv WConv;
struct WConv {
	WChar (*nextfn)(WConv *);
	const char *bp;
	const char *ep;
	const char *cp;
	mbstate_t mbs;
};

static WChar
wconv_nextbyte(WConv *wc)
{
	return wc->cp != wc->ep ? (unsigned char) *wc->cp++ : (WChar) End;
}

static WChar
wconv_nextmbtowc(WConv *wc)
{
	wchar_t wct = L'\0';
	if (wc->cp != wc->ep) {
		size_t n = mbrtowc(&wct, wc->cp, wc->ep - wc->cp, &wc->mbs);
		if (n == 0 || n == (size_t) -1 || n == (size_t) -2) {
			if (wct == L'\0')
				wct = INT32_MIN + (unsigned char) *wc->cp++;
		} else {
			wc->cp += n;
		}
		return wct;
	} else {
		return End;
	}
}

static WChar
wconv_nextutf8(WConv *wc)
{
	if (wc->cp != wc->ep) {
		WChar u = (unsigned char) wc->cp[0];
		if (u < 0x80)
			return wc->cp += 1, u;
		if ((u & 0x40) == 0 || wc->cp + 1 == wc->ep)
		error:
			return wc->cp += 1, INT32_MIN + u;
		WChar v = (unsigned char) wc->cp[1];
		if ((v & 0xC0) != 0x80)
			goto error;
		if ((u & 0x20) == 0) {
			WChar r = ((u & 0x1F) << 6) | (v & 0x3F);
			if (r < 0x80)
				goto error;
			return wc->cp += 2, r;
		}
		if (wc->cp + 2 == wc->ep)
			goto error;
		WChar w = (unsigned char) wc->cp[2];
		if ((w & 0xC0) != 0x80)
			goto error;
		if ((u & 0x10) == 0) {
			WChar r = ((u & 0x0F) << 12) | ((v & 0x3F) << 6) | (w & 0x3F);
			if (r < 0x800)
				goto error;
			return wc->cp += 3, r;
		}
		if (wc->cp + 3 == wc->ep)
			goto error;
		WChar x = (unsigned char) wc->cp[3];
		if ((x & 0xC0) != 0x80)
			goto error;
		if ((u & 0x08) != 0)
			goto error;
		WChar r = ((u & 0x07) << 18) | ((v & 0x3F) << 12) | ((w & 0x3F) << 6) | (x & 0x3F);
		if (r < 0x010000 || r > 0x10FFFF)
			goto error;
		return wc->cp += 4, r;
	} else {
		return End;
	}
}

static WChar (*const wconv_nextfns[3])(WConv *) = { &wconv_nextbyte, &wconv_nextmbtowc, &wconv_nextutf8 };

static void
wconv_construct(WConv *wc, WConv_Encoding e, const char *bp, const char *ep)
{
	wc->nextfn = wconv_nextfns[(int) e];
	wc->bp = bp;
	wc->ep = ep;
	wc->cp = bp;
	memset(&wc->mbs, 0, sizeof wc->mbs);
}

static void
wconv_constructz(WConv *wc, WConv_Encoding e, const char *bp)
{
	wconv_construct(wc, e, bp, bp + strlen(bp));
}

static WChar
wconv_nextchr(WConv *wc)
{
	return wc->nextfn(wc);
}

static WChar
wconv_lookahead(WConv *wc)
{
	WConv wconv = *wc;
	return wconv_nextchr(&wconv);
}

static size_t
wconv_off(const WConv *wc)
{
	return wc->cp - wc->bp;
}

static const char *
wconv_ptr(const WConv *wc)
{
	return wc->cp;
}

static const char *
wconv_save(const WConv *wc)
{
	return wc->cp;
}

static void
wconv_restore(WConv *wc, const char *p)
{
	wc->cp = p;
}

typedef struct FirstBytes FirstBytes;
struct FirstBytes {
	bool vec[256];
};

typedef struct CSet CSet;
struct CSet {
	charset_t *charset;
};

static void
cset_construct(CSet *cs, WConv_Encoding enc)
{
	int errcode = 0;
	cs->charset = charset_create(&errcode, MB_CUR_MAX, enc == UTF8);
	// FIXME: handle errcode or error if charset == NULL
}

static void
cset_destruct(CSet *cs)
{
	if (cs->charset) {
		charset_free(cs->charset);
		cs->charset = (charset_t *) NULL;
	}
}

static CSet *
cset_merge(CSet *cs, const CSet *othercs)
{
	charset_merge(cs->charset, othercs->charset);
	return cs;
}

static CSet *
cset_invert(CSet *cs)
{
	int errcode = 0;
	// FIXME: check errcode and return value
	charset_t *newset = charset_invert(cs->charset, &errcode);
	charset_free(cs->charset);
	cs->charset = newset;
	return cs;
}

static CSet *
cset_set_range(CSet *cs, WChar wclo, WChar wchi)
{
	charset_add_range(cs->charset, wclo, wchi);	// FIXME: check for errors
	return cs;
}

static CSet *
cset_set(CSet *cs, WChar wc)
{
	charset_add_char(cs->charset, wc);	// FIXME: check for errors
	return cs;
}

static CSet *
cset_set_ic(CSet *cs, WChar wc)
{
	charset_add_char_ic(cs->charset, wc);	// FIXME: check for errors
	return cs;
}

static bool
cset_test(const CSet *cs, WChar wc)
{
	return charset_in_set(cs->charset, wc);
}

static bool
cset_cclass(CSet *cs, minrx_regcomp_flags_t flags, WConv_Encoding unused, const char *bp, const char *ep)
{
	int result = charset_add_cclass2(cs->charset, bp, ep);
	if ((flags & MINRX_REG_ICASE) != 0) {
		if (ep - bp == 5 && strncmp(bp, "lower", 5) == 0)
			charset_add_cclass(cs->charset, "upper");	// FIXME: check errors
		else if (ep - bp == 5 && strncmp(bp, "upper", 5) == 0)
			charset_add_cclass(cs->charset, "lower");	// FIXME: check errors
	}
	return result == CSET_SUCCESS;
}

static void
cset_add_equiv(CSet *cs, int32_t equiv)
{
	charset_add_equiv(cs->charset, equiv);
}

static minrx_result_t
cset_parse(CSet *cs, minrx_regcomp_flags_t flags, WConv_Encoding enc, WConv *wconv)
{
	WChar wc = wconv_nextchr(wconv);
	bool inv = wc == L'^';
	if (inv)
		wc = wconv_nextchr(wconv);
	for (bool first = true; first || wc != L']'; first = false) {
		if (wc == End)
			return MINRX_REG_EBRACK;
		WChar wclo = wc, wchi = wc;
		wc = wconv_nextchr(wconv);
		if (wclo == L'\\' && (flags & MINRX_REG_BRACK_ESCAPE) != 0) {
			if (wc != End) {
				wclo = wchi = wc;
				wc = wconv_nextchr(wconv);
			} else {
				return MINRX_REG_EESCAPE;
			}
		} else if (wclo == L'[') {
			if (wc == L'.') {
				wc = wconv_nextchr(wconv);
				wclo = wchi = wc;
#ifdef CHARSET_NOT_YET
				int32_t coll[2] = { wc, L'\0' };
				charset_add_collate(cs->charset, coll);	// FIXME: check errors
				if ((flags & MINRX_REG_ICASE) != 0) {
					if (iswlower(wc))
						coll[0] = towupper(wc);
					else if (iswupper(wc))
						coll[0] = towlower(wc);
					charset_add_collate(cs->charset, coll);	// FIXME: check errors
				}
#endif
				wc = wconv_nextchr(wconv);
				if (wc != L'.' || (wc = wconv_nextchr(wconv)) != L']')
					return MINRX_REG_ECOLLATE;
				wc = wconv_nextchr(wconv);
			} else if (wc == L':') {
				const char *bp = wconv_ptr(wconv), *ep = bp;
				do
					ep = wconv_ptr(wconv), wc = wconv_nextchr(wconv);
				while (wc != End && wc != L':');
				if (wc != L':')
					return MINRX_REG_ECTYPE;
				wc = wconv_nextchr(wconv);
				if (wc != L']')
					return MINRX_REG_ECTYPE;
				wc = wconv_nextchr(wconv);
				if (cset_cclass(cs, flags, enc, bp, ep))
					continue;
				return MINRX_REG_ECTYPE;
			} else if (wc == L'=') {
				wc = wconv_nextchr(wconv);
				wclo = wchi = wc;
				cset_add_equiv(cs, wc);	// FIXME: check errors
				if ((flags & MINRX_REG_ICASE) != 0) {
					if (iswlower(wc))
						cset_add_equiv(cs, towupper(wc));	// FIXME: check errors
					else if (iswupper(wc))
						cset_add_equiv(cs, towlower(wc));	// FIXME: check errors;
				}
				wc = wconv_nextchr(wconv);
				if (wc != L'=' || (wc = wconv_nextchr(wconv)) != L']')
					return MINRX_REG_ECOLLATE;
				wc = wconv_nextchr(wconv);
			}
		}
		bool range = false;
		if (wc == L'-') {
			const char *save = wconv_save(wconv);
			wc = wconv_nextchr(wconv);
			if (wc == End)
				return MINRX_REG_EBRACK;
			if (wc != L']') {
				wchi = wc;
				wc = wconv_nextchr(wconv);
				if (wchi == L'\\' && (flags & MINRX_REG_BRACK_ESCAPE) != 0) {
					if (wc != End) {
						wchi = wc;
						wc = wconv_nextchr(wconv);
					} else {
						return MINRX_REG_EESCAPE;
					}
				} else if (wchi == L'[') {
					if (wc == L'.') {
						wchi = wconv_nextchr(wconv);
						wc = wconv_nextchr(wconv);
						if (wc != L'.' || (wc = wconv_nextchr(wconv)) != L']')
							return MINRX_REG_ECOLLATE;
						wc = wconv_nextchr(wconv);
					} else if (wc == L':' || wc == L'=') {
						return MINRX_REG_ERANGE; // can't be range endpoint
					}
				}
				range = true;
			} else {
				wconv_restore(wconv, save);
				wc = L'-';
			}
		}
		if (wclo > wchi || (wclo != wchi && (wclo < 0 || wchi < 0)))
			return MINRX_REG_ERANGE;
		if (wclo >= 0) {
			cset_set_range(cs, wclo, wchi);
			if ((flags & MINRX_REG_ICASE) != 0) {
				for (WChar wc = wclo; wc <= wchi; ++wc) {
					cset_set(cs, enc == Byte ? tolower(wc) : towlower(wc));
					cset_set(cs, enc == Byte ? toupper(wc) : towupper(wc));
				}
			}
		}
		if (range && wc == L'-' && wconv_lookahead(wconv) != L']')
			return MINRX_REG_ERANGE;
	}
	if (inv) {
		if ((flags & MINRX_REG_NEWLINE) != 0)
			cset_set(cs, L'\n');
		cset_invert(cs);
	}
	return MINRX_REG_SUCCESS;
}

static bool
cset_firstbytes(const CSet *cs, FirstBytes *fb, int32_t *fu, WConv_Encoding e)
{
	for (int i = 0; i < 256; i++)
		fb->vec[i] = false;
	if (e == Byte || e == UTF8) {
		int errcode = 0;
		charset_firstbytes_t bytes = charset_firstbytes(cs->charset, &errcode);
		for (int i = 0; i < MAX_FIRSTBYTES; i++)
			fb->vec[i] = bytes.bytes[i];
	} else {
		return false;
	}
	int n = 0, u = -1;
	for (int i = 0; i < 256; ++i)
		if (fb->vec[i])
			++n, u = i;
	*fu = (n == 1) ? u : -1;
	return true;
}

typedef struct Node Node;
struct Node {
	NInt type;
	NInt args[2];
	NInt nstk;
};

enum NodeType {
	// character-matching node types
	/* char <= uchar max */	// no args
	Cset = WCharMax + 1,	// args = index in Regexp csets vector
	// epsilon-matching node types
	Exit,			// no args
	Fork,			// args = offset to first goto
	Goto,			// args = offset to next goto, offset to just after join
	Join,			// args = none (but supplies incoming stack depth for next node)
	Loop,			// args = offset to next, optional flag
	MinB,			// args = this minified subexpression nesting depth
	MinL,			// args = this minified subexpression nesting depth
	MinR,			// args = this minified subexpression nesting depth
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

typedef struct CSets CSets;
struct CSets {
	CSet *data;
	size_t alloc;
	size_t count;
};

static void
csets_construct(CSets *csets)
{
	csets->data = (CSet *) NULL;
	csets->alloc = 0;
	csets->count = 0;
}

static bool
csets_emplace_back(CSets *csets, WConv_Encoding e)
{
	if (csets->alloc == 0) {
		CSet *newdata = (CSet *) malloc(sizeof (CSet));
		if (!newdata)
			return false;
		csets->data = newdata;
		csets->alloc = 1;
	} else if (csets->alloc == csets->count) {
		size_t newalloc = csets->alloc * 2;
		// N.B. here we are relying on CSet being POD to avoid calling constructors or destructors
		CSet *newdata = (CSet *) realloc((void *) csets->data, newalloc * sizeof (CSet));
		if (!newdata)
			return false;
		csets->data = newdata;
		csets->alloc *= 2;
	}
	cset_construct(&csets->data[csets->count++], e);
	return true;
}

static void
csets_clear(CSets *csets)
{
	if (csets->data) {
		for (size_t i = 0, n = csets->count; i < n; i++)
			cset_destruct(&csets->data[i]);
		free((void *) csets->data);
		csets->data = (CSet *) NULL;
		csets->alloc = 0;
		csets->count = 0;
	}
}

static const CSet *
csets_aref(const CSets *csets, NInt i)
{
	return &csets->data[i];
}

static CSet *
csets_back(CSets *csets)
{
	return &csets->data[csets->count - 1];
}

static size_t
csets_size(CSets *csets)
{
	return csets->count;
}

typedef struct Regexp Regexp;
struct Regexp {
	WConv_Encoding enc;
	minrx_result_t err;
	CSets csets;
	const Node *nodes;
	size_t nnode;
	CSet *firstcset;
	bool firstvalid;
	FirstBytes firstbytes;
	int32_t firstunique;
	size_t nmin;
	size_t nstk;
	size_t nsub;
};

//
// The output of compiling is a malloced array of nodes, but while compiling
// we keep the future array elements in a singly-linked list to facilitate
// concatenation as well as element insertion at both ends.
//
typedef struct ListNode ListNode;	// Node wrapped in list element container.
typedef struct NodeList NodeList;	// List of nodes.
typedef struct NodePool NodePool;	// Pool of not-yet-allocated ListNodes.

struct ListNode {
	Node node;
	ListNode *next;
};

struct NodeList {
	ListNode *first;
	ListNode *final;
	NInt size;
};

#define NALLOC 25
struct NodePool {
	int nalloc;
	NodePool *prev;
	ListNode nodes[NALLOC];
};

static ListNode *
alloc(NodePool **npp, NInt type, NInt arg0, NInt arg1, NInt nstk)
{
	ListNode *n;
	if (!*npp || (*npp)->nalloc == NALLOC) {
		NodePool *np = (NodePool *) malloc(sizeof (NodePool));
		if (!np)
			return (ListNode *) NULL;
		np->nalloc = 0;
		np->prev = *npp;
		*npp = np;
	}
	n = &(*npp)->nodes[(*npp)->nalloc++];
	n->node.type = type;
	n->node.args[0] = arg0;
	n->node.args[1] = arg1;
	n->node.nstk = nstk;
	return n;
}

static bool
concatcopy(NodePool **npp, NodeList *x, const NodeList *y)
{
	for (ListNode *n = y->first; n; n = n->next) {
		ListNode *nn = alloc(npp, n->node.type, n->node.args[0], n->node.args[1], n->node.nstk);
		if (!nn)
			return false;
		if (x->size++)
			x->final->next = nn;
		else
			x->first = nn;
		x->final = nn;
		nn->next = (ListNode *) NULL;
	}
	return true;
}

static void
concatmove(NodeList *x, const NodeList *y)
{
	if (x->size) {
		if (y->size)
			x->final->next = y->first, x->final = y->final, x->size += y->size;
	} else {
		*x = *y;
	}
}

static bool
emplace_first(NodePool **npp, NodeList *x, NInt type, NInt arg0, NInt arg1, NInt nstk)
{
	ListNode *n = alloc(npp, type, arg0, arg1, nstk);
	if (!n)
		return false;
	if (x->size++) {
		n->next = x->first;
		x->first = n;
	} else {
		n->next = (ListNode *) NULL;
		x->first = x->final = n;
	}
	return true;
}

static bool
emplace_final(NodePool **npp, NodeList *x, NInt type, NInt arg0, NInt arg1, NInt nstk)
{
	ListNode *n = alloc(npp, type, arg0, arg1, nstk);
	if (!n)
		return false;
	if (x->size++)
		x->final->next = n;
	else
		x->first = n;
	x->final = n;
	n->next = (ListNode *) NULL;
	return true;
}

static NodeList
empty()
{
	NodeList r;
	r.first = (ListNode *) NULL;
	r.final = (ListNode *) NULL;
	r.size = 0;
	return r;
}

typedef struct Compile Compile;
struct Compile {
	minrx_regcomp_flags_t flags;
	WConv_Encoding enc;
	WConv wconv;
	WChar wc;
	CSets csets;
	size_t dot;
	size_t esc_s;
	size_t esc_S;
	size_t esc_w;
	size_t esc_W;
	NInt nmin;
	NInt nsub;
	NodePool *np;
};

static NInt
satmul(NInt x, NInt y)
{
	if (x == 0 || y == 0)
		return 0;
	NInt z = x * y;
	return z / y == x ? z : (NInt) -1;
}

static bool
num(Compile *c, NInt *np, WChar *wcp)
{
	if (*wcp < L'0' || *wcp > L'9')
		return false;
	NInt v = 0;
	do {
		v = satmul(v, 10);
		if (v == (NInt) -1 || (v += *wcp - L'0') < (NInt) *wcp - L'0') {
			do
				*wcp = wconv_nextchr(&c->wconv);
			while (*wcp >= L'0' && *wcp <= L'9');
			*np = -1;
			return true;
		}
		*wcp = wconv_nextchr(&c->wconv);
	} while (*wcp >= L'0' && *wcp <= L'9');
	*np = v;
	return true;
}

typedef struct Subexp Subexp;
struct Subexp {
	NodeList nodes;
	size_t maxstk;
	bool hasmin;
	minrx_result_t err;
};

static Subexp cat(Compile *, bool, NInt);
static Subexp
alt(Compile *c, bool nested, NInt nstk)
{
	Subexp lh = cat(c, nested, nstk);
	if (lh.err != MINRX_REG_SUCCESS)
		return lh;
	if (c->wc == L'|') {
		for (ListNode *l = lh.nodes.first; l != (ListNode *) NULL; l = l->next)
			l->node.nstk += 1;
		Subexp altspace[16], *alts = altspace;
		size_t alloc = 16, count = 0;
		while (c->wc == L'|') {
			c->wc = wconv_nextchr(&c->wconv);
			if (count == alloc) {
				alloc *= 2;
				if (alts == altspace) {
					Subexp *newalts = (Subexp *) malloc(alloc * sizeof (Subexp));	// FIXME: check return value
					memcpy((void *) newalts, (void *) alts, count * sizeof (Subexp));
					alts = newalts;
				} else {
					Subexp *newalts = (Subexp *) realloc((void *) alts, alloc * sizeof (Subexp));
					alts = newalts;
				}
			}
			alts[count++] = cat(c, nested, nstk + 1);
		}
		Subexp rh = alts[count - 1];
		if (rh.err != MINRX_REG_SUCCESS)
			return rh;
		if (!emplace_first(&c->np, &rh.nodes, Goto, rh.nodes.size, rh.nodes.size + 1, nstk + 1))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		--count;
		while (count > 0) {
			Subexp mh = alts[--count];
			NInt mhsize = mh.nodes.size;
			concatmove(&mh.nodes, &rh.nodes);
			rh.maxstk = MAX(mh.maxstk, rh.maxstk);
			rh.hasmin |= mh.hasmin;
			rh.nodes = mh.nodes;
			if (!emplace_first(&c->np, &rh.nodes, Goto, mhsize, rh.nodes.size + 1, nstk + 1))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		}
		if (alts != altspace)
			free((void *) alts);
		if (!emplace_first(&c->np, &lh.nodes, Fork, lh.nodes.size, 0, nstk + 1))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		concatmove(&lh.nodes, &rh.nodes);
		lh.maxstk = MAX(lh.maxstk, rh.maxstk);
		lh.hasmin |= rh.hasmin;
		if (!emplace_final(&c->np, &lh.nodes, Join, lh.nodes.size - 1, 0, nstk + 1))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
	}
	return LITERAL(Subexp) {lh.nodes, lh.maxstk, lh.hasmin, MINRX_REG_SUCCESS};
}

static Subexp rep(Compile *, bool, NInt);
static Subexp
cat(Compile *c, bool nested, NInt nstk)
{
	Subexp lh = rep(c, nested, nstk);
	if (lh.err != MINRX_REG_SUCCESS)
		return lh;
	while (c->wc != End && c->wc != L'|' && (c->wc != L')' || !nested)) {
		Subexp rh = rep(c, nested, nstk);
		if (rh.err != MINRX_REG_SUCCESS)
			return rh;
		concatmove(&lh.nodes, &rh.nodes);
		lh.maxstk = MAX(lh.maxstk, rh.maxstk);
		lh.hasmin |= rh.hasmin;
	}
	return lh;
}

static Subexp
minimize(Compile *c, Subexp lh, NInt nstk)
{
	if (lh.err != MINRX_REG_SUCCESS)
		return lh;
	for (ListNode *n = lh.nodes.first; n != (ListNode *) NULL; n = n->next)
		n->node.nstk += 1;
	if (!emplace_first(&c->np, &lh.nodes, MinL, 0, 0, nstk + 1) || !emplace_final(&c->np, &lh.nodes, MinR, 0, 0, nstk))
		return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
	c->nmin = MAX(c->nmin, (size_t) 1);
	return LITERAL(Subexp) {lh.nodes, lh.maxstk + 1, true, lh.err};
}

static void
minraise(Compile *c, Subexp *lhp)
{
	if (lhp->err != MINRX_REG_SUCCESS)
		return;
	NInt maxlevel = 0;
	for (ListNode *n = lhp->nodes.first; n != (ListNode *) NULL; n = n->next)
		switch (n->node.type) {
		case MinB:
		case MinL:
		case MinR:
			maxlevel = MAX(maxlevel, ++n->node.args[0]);
			break;
		default:
			;
		}
	c->nmin = MAX(c->nmin, maxlevel + 1);
}

static Subexp
mkrep(Compile *c, Subexp lh, bool optional, bool infinite, NInt nstk)
{
	if (lh.err != MINRX_REG_SUCCESS)
		return lh;
	if (optional && !infinite) {
		for (ListNode *l = lh.nodes.first; l != (ListNode *) NULL; l = l->next)
			l->node.nstk += 2;
		NInt lhsize = lh.nodes.size;
		if (!emplace_first(&c->np, &lh.nodes, Skip, lhsize, 0, nstk + 2))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		return LITERAL(Subexp) {lh.nodes, lh.maxstk + 2, lh.hasmin, MINRX_REG_SUCCESS};
	} else {
		for (ListNode *l = lh.nodes.first; l != (ListNode *) NULL; l = l->next)
			l->node.nstk += 3;
		NInt lhsize = lh.nodes.size;
		if (!emplace_first(&c->np, &lh.nodes, Loop, lhsize, (NInt) optional, nstk + 3) || !emplace_final(&c->np, &lh.nodes, Next, lhsize, (NInt) infinite, nstk))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		return LITERAL(Subexp) {lh.nodes, lh.maxstk + 3, lh.hasmin, MINRX_REG_SUCCESS};
	}
}

static Subexp
mkrep_braces(Compile *c, Subexp lh, NInt m, NInt n, NInt nstk)
{
	if (lh.err != MINRX_REG_SUCCESS)
		return lh;
	if ((m != (NInt) -1 && m > RE_DUP_MAX) || (n != (NInt) -1 && n > RE_DUP_MAX) || m > n)
		return LITERAL(Subexp) {{}, 0, false, MINRX_REG_BADBR};
	if (n == 0)
		return LITERAL(Subexp) {{}, 0, false, MINRX_REG_SUCCESS};
	if (m == 0 && n == 1)
		return mkrep(c, lh, true, false, nstk);
	if (m == 0 && n == (NInt) -1)
		return mkrep(c, lh, true, true, nstk);
	if (m == 1 && n == 1)
		return lh;
	if (m == 1 && n == (NInt) -1)
		return mkrep(c, lh, false, true, nstk);
	Subexp rh = lh;
	NInt k;
	lh.nodes = empty();
	if (!concatcopy(&c->np, &lh.nodes, &rh.nodes))	// force initial lhs to be a copy
		return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
	for (k = 1; k < m; ++k)
		if (!concatcopy(&c->np, &lh.nodes, &rh.nodes))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
	if (n != (NInt) -1 && k < n) {
		lh.maxstk += 2;
		rh.maxstk += 2;
		for (ListNode *r = rh.nodes.first; r != (ListNode *) NULL; r = r->next)
			r->node.nstk += 2;
		NInt rhsize = rh.nodes.size;
		if (!emplace_first(&c->np, &rh.nodes, Skip, rhsize, 1, nstk + 2))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		for (; k < n; ++k)
			if (!concatcopy(&c->np, &lh.nodes, &rh.nodes))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
	}
	if (n == (NInt) -1) {
		lh.maxstk += 3;
		rh.maxstk += 3;
		for (ListNode *r = rh.nodes.first; r != (ListNode *) NULL; r = r->next)
			r->node.nstk += 3;
		NInt rhsize = rh.nodes.size;
		if (!emplace_first(&c->np, &rh.nodes, Loop, rhsize, 1, nstk + 3) || !emplace_final(&c->np, &rh.nodes, Next, rhsize, 1, nstk))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		concatmove(&lh.nodes, &rh.nodes);
	}
	if (m == 0)
		return mkrep(c, LITERAL(Subexp) {lh.nodes, rh.maxstk, rh.hasmin, MINRX_REG_SUCCESS}, true, false, nstk);
	else
		return LITERAL(Subexp) {lh.nodes, rh.maxstk, rh.hasmin, MINRX_REG_SUCCESS};
}

static Subexp chr(Compile *, bool, NInt);
static Subexp
rep(Compile *c, bool nested, NInt nstk)
{
	Subexp lh = chr(c, nested, nstk);
	if (lh.err != MINRX_REG_SUCCESS)
		return lh;
	bool hasmin = lh.hasmin;
	for (;;) {
		bool infinite = false, minimal = (c->flags & MINRX_REG_MINIMAL) != 0, optional = false;
		switch (c->wc) {
		case L'?':
			optional = true;
			goto common;
		case L'*':
			infinite = optional = true;
			goto common;
		case L'+':
			infinite = true;
		common:	if ((c->flags & MINRX_REG_MINDISABLE) == 0 && (c->wc = wconv_nextchr(&c->wconv)) == L'?')
				minimal ^= true, c->wc = wconv_nextchr(&c->wconv);
			if (hasmin)
				minraise(c, &lh);
			lh = mkrep(c, minimal ? minimize(c, lh, nstk) : lh, optional, infinite, nstk);
		comout:	if (minimal) {
				if (!emplace_first(&c->np, &lh.nodes, MinB, 0, 0, nstk))
					return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
				hasmin = true;
			}
			lh.hasmin = hasmin;
			continue;
		case L'{':
			if ((c->flags & MINRX_REG_BRACE_COMPAT) == 0
			    || (c->enc == Byte ? isdigit(wconv_lookahead(&c->wconv)) : IsDigit(wconv_lookahead(&c->wconv))))
			{
				c->wc = wconv_nextchr(&c->wconv);
				if (c->wc == End)
					return LITERAL(Subexp) {{}, 0, false, MINRX_REG_EBRACE};
				NInt m, n;
				if (!num(c, &m, &c->wc))
					return LITERAL(Subexp) {{}, 0, false, MINRX_REG_BADBR};
				if (c->wc == L'}') {
					if ((c->flags & MINRX_REG_MINDISABLE) == 0 && (c->wc = wconv_nextchr(&c->wconv)) == L'?')
						minimal ^= true, c->wc = wconv_nextchr(&c->wconv);
					if (hasmin)
						minraise(c, &lh);
					lh = mkrep_braces(c, minimal ? minimize(c, lh, nstk) : lh, m, m, nstk);
					goto comout;
				}
				if (c->wc == End)
					return LITERAL(Subexp) {{}, 0, false, MINRX_REG_EBRACE};
				if (c->wc != L',')
					return LITERAL(Subexp) {{}, 0, false, MINRX_REG_BADBR};
				c->wc = wconv_nextchr(&c->wconv);
				if (c->wc == L'}') {
					if ((c->flags & MINRX_REG_MINDISABLE) == 0 && (c->wc = wconv_nextchr(&c->wconv)) == L'?')
						minimal ^= true, c->wc = wconv_nextchr(&c->wconv);
					if (hasmin)
						minraise(c, &lh);
					lh = mkrep_braces(c, minimal ? minimize(c, lh, nstk) : lh, m, -1, nstk);
					goto comout;
				}
				if (!num(c, &n, &c->wc))
					return LITERAL(Subexp) {{}, 0, false, MINRX_REG_BADBR};
				if (c->wc == End)
					return LITERAL(Subexp) {{}, 0, false, MINRX_REG_EBRACE};
				if (c->wc != L'}')
					return LITERAL(Subexp) {{}, 0, false, MINRX_REG_BADBR};
				if ((c->flags & MINRX_REG_MINDISABLE) == 0 && (c->wc = wconv_nextchr(&c->wconv)) == L'?')
					minimal ^= true, c->wc = wconv_nextchr(&c->wconv);
				if (hasmin)
					minraise(c, &lh);
				lh = mkrep_braces(c, minimal ? minimize(c, lh, nstk) : lh, m, n, nstk);
				goto comout;
			}
			// fall through
		default:
			return lh;
		}
	}
}

static Subexp
chr(Compile *c, bool nested, NInt nstk)
{
	NodeList lhs = empty();
	size_t lhmaxstk;
	bool lhasmin = false;
	switch (c->wc) {
	default:
	normal:
		lhmaxstk = nstk;
		if ((c->flags & MINRX_REG_ICASE) == 0) {
			if (!emplace_first(&c->np, &lhs, (NInt) c->wc, 0, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		} else {
			csets_emplace_back(&c->csets, c->enc);	// FIXME: check for error
			cset_set_ic(csets_back(&c->csets), c->wc);
			if (!emplace_final(&c->np, &lhs, Cset, csets_size(&c->csets) - 1, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		}
		c->wc = wconv_nextchr(&c->wconv);
		break;
	case L'{':
		if ((c->flags & MINRX_REG_BRACE_COMPAT) != 0
		    && (c->enc == Byte ? !isdigit(wconv_lookahead(&c->wconv)) : !IsDigit(wconv_lookahead(&c->wconv))))
			goto normal;
		// fall through
	case L'*':
	case L'+':
	case L'?':
		return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_BADRPT};
	case L'[':
		lhmaxstk = nstk;
		if (!emplace_final(&c->np, &lhs, Cset, csets_size(&c->csets), 0, nstk))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		csets_emplace_back(&c->csets, c->enc);		// FIXME: check for error
		{
			minrx_result_t err = cset_parse(csets_back(&c->csets), c->flags, c->enc, &c->wconv);
			if (err)
				return LITERAL(Subexp) {empty(), 0, false, err};
		}
		c->wc = wconv_nextchr(&c->wconv);
		break;
	case L'.':
		if (c->dot == (size_t) -1) {
			c->dot = csets_size(&c->csets);
			csets_emplace_back(&c->csets, c->enc);
			if ((c->flags & MINRX_REG_NEWLINE) != 0)
				cset_set(csets_back(&c->csets), L'\n');
			cset_invert(csets_back(&c->csets));
		}
		lhmaxstk = nstk;
		if (!emplace_final(&c->np, &lhs, Cset, c->dot, 0, nstk))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		c->wc = wconv_nextchr(&c->wconv);
		break;
	case L'^':
		lhmaxstk = nstk;
		if (!emplace_final(&c->np, &lhs, (c->flags & MINRX_REG_NEWLINE) == 0 ? ZBOB : ZBOL, 0, 0, nstk))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		c->wc = wconv_nextchr(&c->wconv);
		break;
	case L'$':
		lhmaxstk = nstk;
		if (!emplace_final(&c->np, &lhs, (c->flags & MINRX_REG_NEWLINE) == 0 ? ZEOB : ZEOL, 0, 0, nstk))
			return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
		c->wc = wconv_nextchr(&c->wconv);
		break;
	case L'\\':
		lhmaxstk = nstk;
		c->wc = wconv_nextchr(&c->wconv);
		switch (c->wc) {
		case L'<':
			if ((c->flags & MINRX_REG_EXTENSIONS_BSD) == 0)
				goto normal;
			if (!emplace_final(&c->np, &lhs, ZBOW, 0, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case L'>':
			if ((c->flags & MINRX_REG_EXTENSIONS_BSD) == 0)
				goto normal;
			if (!emplace_final(&c->np, &lhs, ZEOW, 0, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case L'`':
			if ((c->flags & MINRX_REG_EXTENSIONS_GNU) == 0)
				goto normal;
			if (!emplace_final(&c->np, &lhs, ZBOB, 0, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case L'\'':
			if ((c->flags & MINRX_REG_EXTENSIONS_GNU) == 0)
				goto normal;
			if (!emplace_final(&c->np, &lhs, ZEOB, 0, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case L'b':
			if ((c->flags & MINRX_REG_EXTENSIONS_GNU) == 0)
				goto normal;
			if (!emplace_final(&c->np, &lhs, ZXOW, 0, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case L'B':
			if ((c->flags & MINRX_REG_EXTENSIONS_GNU) == 0)
				goto normal;
			if (!emplace_final(&c->np, &lhs, ZNWB, 0, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case L's':
			if ((c->flags & MINRX_REG_EXTENSIONS_GNU) == 0)
				goto normal;
			if (c->esc_s == (size_t) -1) {
				c->esc_s = csets_size(&c->csets);
				WConv wconv;
				wconv_constructz(&wconv, c->enc, "[:space:]]");
				csets_emplace_back(&c->csets, c->enc);		// FIXME: check for error
				cset_parse(csets_back(&c->csets), c->flags, c->enc, &wconv);
			}
			if (!emplace_final(&c->np, &lhs, Cset, c->esc_s, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case L'S':
			if ((c->flags & MINRX_REG_EXTENSIONS_GNU) == 0)
				goto normal;
			if (c->esc_S == (size_t) -1) {
				c->esc_S = csets_size(&c->csets);
				WConv wconv;
				wconv_constructz(&wconv, c->enc, "^[:space:]]");
				csets_emplace_back(&c->csets, c->enc);		// FIXME: check for error
				cset_parse(csets_back(&c->csets), c->flags, c->enc, &wconv);
			}
			if (!emplace_final(&c->np, &lhs, Cset, c->esc_S, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case L'w':
			if ((c->flags & MINRX_REG_EXTENSIONS_GNU) == 0)
				goto normal;
			if (c->esc_w == (size_t) -1) {
				c->esc_w = csets_size(&c->csets);
				WConv wconv;
				wconv_constructz(&wconv, c->enc, "[:alnum:]_]");
				csets_emplace_back(&c->csets, c->enc);		// FIXME: check for error
				cset_parse(csets_back(&c->csets), c->flags, c->enc, &wconv);
			}
			if (!emplace_final(&c->np, &lhs, Cset, c->esc_w, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case L'W':
			if ((c->flags & MINRX_REG_EXTENSIONS_GNU) == 0)
				goto normal;
			if (c->esc_W == (size_t) -1) {
				c->esc_W = csets_size(&c->csets);
				WConv wconv;
				wconv_constructz(&wconv, c->enc, "^[:alnum:]_]");
				csets_emplace_back(&c->csets, c->enc);		// FIXME: check for error
				cset_parse(csets_back(&c->csets), c->flags, c->enc, &wconv);
			}
			if (!emplace_final(&c->np, &lhs, Cset, c->esc_W, 0, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			break;
		case End:
			return LITERAL(Subexp) {{}, 0, false, MINRX_REG_EESCAPE};
		default:
			goto normal;
		}
		c->wc = wconv_nextchr(&c->wconv);
		break;
	case L'(':
		{
			NInt n = ++c->nsub;
			c->wc = wconv_nextchr(&c->wconv);
			minrx_result_t err;
			Subexp lh = alt(c, true, nstk + 1);
			lhs = lh.nodes;
			lhmaxstk = lh.maxstk;
			lhasmin = lh.hasmin;
			err = lh.err;
			if (err)
				return LITERAL(Subexp) {lhs, lhmaxstk, lhasmin, err};
			if (c->wc != L')')
				return LITERAL(Subexp) {{}, 0, false, MINRX_REG_EPAREN};
			if (!emplace_first(&c->np, &lhs, SubL, n, c->nsub, nstk + 1) || !emplace_final(&c->np, &lhs, SubR, n, c->nsub, nstk))
				return LITERAL(Subexp) {empty(), 0, false, MINRX_REG_ESPACE};
			c->wc = wconv_nextchr(&c->wconv);
		}
		break;
	case L')':
		if (!nested)
			goto normal;
		// fall through
	case L'|':
	case End:
		lhmaxstk = nstk;
		break;
	}
	return LITERAL(Subexp) {lhs, lhmaxstk, lhasmin, MINRX_REG_SUCCESS};
}

static void
firstinsert(QSet *q, QSet *v, NInt n)
{
	if (!qset_contains(v, n)) {
		qset_insert(q, n);
		qset_insert(v, n);
	}
}

static CSet *
firstclosure(Compile *c, const Node *nodes, NInt nnode)
{
	if (nnode == 0)
		return (CSet *) NULL;
	QSet epsq, epsv, firsts;
	if (!qset_construct(&epsq, nnode))
		return (CSet *) NULL;
	if (!qset_construct(&epsv, nnode)) {
		qset_destruct(&epsq);
		return (CSet *) NULL;
	}
	if (!qset_construct(&firsts, nnode)) {
		qset_destruct(&epsq);
		qset_destruct(&epsv);
		return (CSet *) NULL;
	}
	qset_insert(&epsq, 0);
	do {
		NInt k = qset_remove(&epsq);
		const Node *np = &nodes[k];
		if (np->type <= Cset)
			qset_insert(&firsts, k);
		else
			switch (np->type) {
			case Exit:
				qset_destruct(&firsts);
				qset_destruct(&epsv);
				qset_destruct(&epsq);
				return (CSet *) NULL;
			case Fork:
				do {
					firstinsert(&epsq, &epsv, k + 1);
					k = k + 1 + nodes[k].args[0];
				} while (nodes[k].type != Join);
				break;
			case Goto:
				firstinsert(&epsq, &epsv, k + 1 + np->args[0]);
				break;
			case Loop:
				firstinsert(&epsq, &epsv, k + 1);
				if (np->args[1])
					firstinsert(&epsq, &epsv, k + 1 + np->args[0]);
				break;
			case Skip:
				firstinsert(&epsq, &epsv, k + 1);
				firstinsert(&epsq, &epsv, k + 1 + np->args[0]);
				break;
			default:
				firstinsert(&epsq, &epsv, k + 1);
				break;
			}
	} while (!qset_empty(&epsq));
	CSet *cs = (CSet *) malloc(sizeof (CSet));
	cset_construct(cs, c->enc);
	while (!qset_empty(&firsts)) {
		NInt k = qset_remove(&firsts);
		NInt t = nodes[k].type;
		if (t <= WCharMax)
			cset_set(cs, t);
		else
			cset_merge(cs, csets_aref(&c->csets, nodes[k].args[0]));
	}
	qset_destruct(&firsts);
	qset_destruct(&epsv);
	qset_destruct(&epsq);
	return cs;
}

static bool
firstbytes(FirstBytes *fb, int32_t *fu, WConv_Encoding e, const CSet *firstcset)
{
	if (!firstcset)
		return false;
	return cset_firstbytes(firstcset, fb, fu, e);
}

static Regexp *
compile(Compile *c)
{
	Node *nodes = (Node *) NULL;
	NInt nnode = 0;
	if ((c->flags & MINRX_REG_MINDISABLE) != 0 && (c->flags & MINRX_REG_MINIMAL) != 0) {
		Regexp *r = (Regexp *) malloc(sizeof (Regexp));
		if (r) {
			r->enc = c->enc;
			r->err = MINRX_REG_BADPAT;
			csets_construct(&r->csets);
			r->nodes = NULL;
			r->nnode = 0;
			r->firstcset = NULL;
			r->firstvalid = false;
			memset(&r->firstbytes, 0, sizeof r->firstbytes);
			r->firstunique = -1;
			r->nmin = 0;
			r->nstk = 0;
			r->nsub = 0;
		}
		return r;
	}
	Subexp lh = alt(c, false, 0);
	if (lh.err == MINRX_REG_SUCCESS && (!emplace_final(&c->np, &lh.nodes, Exit, 0, 0, 0) || !(nodes = (Node *) malloc(lh.nodes.size * sizeof (Node)))))
		lh.err = MINRX_REG_ESPACE;
	if (lh.err != MINRX_REG_SUCCESS) {
		csets_clear(&c->csets);
		lh.nodes = empty();
		c->nmin = 0;
		lh.maxstk = 0;
		c->nsub = 0;
	}
	if (c->nmin > 0)
		for (ListNode *l = lh.nodes.first; l; l = l->next)
			l->node.nstk += c->nmin;
	for (ListNode *l = lh.nodes.first; l; l = l->next)
		nodes[nnode++] = l->node;
	for (NodePool *p = c->np, *q; p; p = q) {
		q = p->prev;
		free((void *) p);
	}
	c->np = (NodePool *) NULL;
	CSet *fc = firstclosure(c, nodes, nnode);		// FIXME: check for allocation errors
	FirstBytes fb;
	int32_t fu = -1;
	bool fv = firstbytes(&fb, &fu, c->enc, fc);
	Regexp *r = (Regexp *) malloc(sizeof (Regexp));
	if (r) {
		r->enc = c->enc;
		r->err = lh.err;
		r->csets = c->csets;
		r->nodes = nodes;
		r->nnode = nnode;
		r->firstcset = fc;
		r->firstvalid = fv;
		r->firstbytes = fb;
		r->firstunique = fu;
		r->nmin = c->nmin;
		r->nstk = lh.maxstk;
		r->nsub = c->nsub + 1;
	}
	return r;
}

static const size_t SizeBits = CHAR_BIT * sizeof (size_t); // FIXME: find the technically correct way to do this

typedef struct Execute Execute;
struct Execute {
	const Regexp *r;
	minrx_regexec_flags_t flags;
	const Node *nodes;
	size_t suboff;
	size_t minvcnt;
	size_t minvoff;
	size_t nestoff;
	size_t gen;
	size_t off;
	WConv wconv;
	WChar wcprev;
	COWVec_Allocator allocator;
	COWVec best;
	NInt bestmincount; // note mincounts are negated so this means +infinity
	QSet epsq;
	QVec epsv;
};

static bool
execute_construct(Execute *e, const Regexp *r, minrx_regexec_flags_t flags, const char *bp, const char *ep)
{
	e->r = r;
	e->flags = flags;
	e->nodes = r->nodes;
	e->suboff = r->nmin + r->nstk;
	e->minvcnt = (r->nmin + SizeBits - 1) / SizeBits;
	e->minvoff = e->suboff + 2 * r->nsub;
	e->nestoff = e->minvoff + e->minvcnt;
	e->gen = 0;
	e->off = 0;
	wconv_construct(&e->wconv, r->enc, bp, ep);
	e->wcprev = End;
	cowvec_allocator_construct(&e->allocator, e->nestoff + r->nmin);
	cowvec_construct(&e->best, (COWVec_Allocator *) NULL);
	e->bestmincount = 0;
	if (!qset_construct(&e->epsq, r->nnode))
		return false;
	qvec_construct(&e->epsv, r->nnode);
	return true;
}

static void
execute_destruct(Execute *e)
{
	qvec_destruct(&e->epsv);
	qset_destruct(&e->epsq);
	cowvec_destruct(&e->best);
	cowvec_allocator_destruct(&e->allocator);
}

inline static void
execute_add(Execute *e, QVec *ncsv, NInt k, NInt nstk, const NState *nsp, WChar wcnext)
{
	const Node *np = &e->nodes[k];
	if (np->type <= Cset) {
		if (np->type == (NInt) wcnext || (np->type == Cset && cset_test(csets_aref(&e->r->csets, np->args[0]), wcnext))) {
			QVecInsert qvi = qvec_insert(ncsv, k, nsp);
			if (qvi.newly) {
				nstate_construct_copy(qvi.newnsp, nsp);
			} else {
				int x = nstate_cmp(nsp, qvi.newnsp, e->gen, nstk);
				if (x > 0 || (x == 0 && e->minvcnt && cowvec_cmp_from(e->minvoff, &nsp->substack, &qvi.newnsp->substack, e->minvcnt) > 0))
					nstate_copy(qvi.newnsp, nsp);
				else
					return;
			}
			qvi.newnsp->gen = e->gen;
		}
	} else {
		QVecInsert qvi = qvec_insert(&e->epsv, k, nsp);
		if (qvi.newly) {
			nstate_construct_copy(qvi.newnsp, nsp);
		} else {
			int x = nstate_cmp(nsp, qvi.newnsp, e->gen, nstk);
			if (x > 0 || (x == 0 && e->minvcnt && cowvec_cmp_from(e->minvoff, &nsp->substack, &qvi.newnsp->substack, e->minvcnt) > 0))
				nstate_copy(qvi.newnsp, nsp);
			else
				return;
		}
		qvi.newnsp->gen = e->gen;
		qset_insert(&e->epsq, k);
	}
}

inline static void
execute_add_1(Execute *e, QVec *ncsv, NInt k, NInt nstk, const NState *nsp, WChar wcnext, NInt arg1)
{
	const Node *np = &e->nodes[k];
	if (np->type <= Cset) {
		if (np->type == (NInt) wcnext || (np->type == Cset && cset_test(csets_aref(&e->r->csets, np->args[0]), wcnext))) {
			QVecInsert qvi = qvec_insert(ncsv, k, nsp);
			if (qvi.newly) {
				nstate_construct_copy(qvi.newnsp, nsp);
			} else {
				int x = nstate_cmp_1(nsp, qvi.newnsp, e->gen, nstk, arg1);
				if (x > 0 || (x == 0 && e->minvcnt && cowvec_cmp_from(e->minvoff, &nsp->substack, &qvi.newnsp->substack, e->minvcnt) > 0))
					nstate_copy(qvi.newnsp, nsp);
				else
					return;
			}
			qvi.newnsp->gen = e->gen;
			cowvec_put(&qvi.newnsp->substack, nstk - 1, arg1);
		}
	} else {
		QVecInsert qvi = qvec_insert(&e->epsv, k, nsp);
		if (qvi.newly) {
			nstate_construct_copy(qvi.newnsp, nsp);
		} else {
			int x = nstate_cmp_1(nsp, qvi.newnsp, e->gen, nstk, arg1);
			if (x > 0 || (x == 0 && e->minvcnt && cowvec_cmp_from(e->minvoff, &nsp->substack, &qvi.newnsp->substack, e->minvcnt) > 0))
				nstate_copy(qvi.newnsp, nsp);
			else
				return;
		}
		qvi.newnsp->gen = e->gen;
		cowvec_put(&qvi.newnsp->substack, nstk - 1, arg1);
		qset_insert(&e->epsq, k);
	}
}

inline static void
execute_add_2(Execute *e, QVec *ncsv, NInt k, NInt nstk, const NState *nsp, WChar wcnext, NInt arg1, NInt arg2)
{
	const Node *np = &e->nodes[k];
	if (np->type <= Cset) {
		if (np->type == (NInt) wcnext || (np->type == Cset && cset_test(csets_aref(&e->r->csets, np->args[0]), wcnext))) {
			QVecInsert qvi = qvec_insert(ncsv, k, nsp);
			if (qvi.newly) {
				nstate_construct_copy(qvi.newnsp, nsp);
			} else {
				int x = nstate_cmp_2(nsp, qvi.newnsp, e->gen, nstk, arg1, arg2);
				if (x > 0 || (x == 0 && e->minvcnt && cowvec_cmp_from(e->minvoff, &nsp->substack, &qvi.newnsp->substack, e->minvcnt) > 0))
					nstate_copy(qvi.newnsp, nsp);
				else
					return;
			}
			qvi.newnsp->gen = e->gen;
			cowvec_put(&qvi.newnsp->substack, nstk - 2, arg1);
			cowvec_put(&qvi.newnsp->substack, nstk - 1, arg2);
		}
	} else {
		QVecInsert qvi = qvec_insert(&e->epsv, k, nsp);
		if (qvi.newly) {
			nstate_construct_copy(qvi.newnsp, nsp);
		} else {
			int x = nstate_cmp_2(nsp, qvi.newnsp, e->gen, nstk, arg1, arg2);
			if (x > 0 || (x == 0 && e->minvcnt && cowvec_cmp_from(e->minvoff, &nsp->substack, &qvi.newnsp->substack, e->minvcnt) > 0))
				nstate_copy(qvi.newnsp, nsp);
			else
				return;
		}
		qvi.newnsp->gen = e->gen;
		cowvec_put(&qvi.newnsp->substack, nstk - 2, arg1);
		cowvec_put(&qvi.newnsp->substack, nstk - 1, arg2);
		qset_insert(&e->epsq, k);
	}
}

inline static void
execute_add_3(Execute *e, QVec *ncsv, NInt k, NInt nstk, const NState *nsp, WChar wcnext, NInt arg1, NInt arg2, NInt arg3)
{
	const Node *np = &e->nodes[k];
	if (np->type <= Cset) {
		if (np->type == (NInt) wcnext || (np->type == Cset && cset_test(csets_aref(&e->r->csets, np->args[0]), wcnext))) {
			QVecInsert qvi = qvec_insert(ncsv, k, nsp);
			if (qvi.newly) {
				nstate_construct_copy(qvi.newnsp, nsp);
			} else {
				int x = nstate_cmp_3(nsp, qvi.newnsp, e->gen, nstk, arg1, arg2, arg3);
				if (x > 0 || (x == 0 && e->minvcnt && cowvec_cmp_from(e->minvoff, &nsp->substack, &qvi.newnsp->substack, e->minvcnt) > 0))
					nstate_copy(qvi.newnsp, nsp);
				else
					return;
			}
			qvi.newnsp->gen = e->gen;
			cowvec_put(&qvi.newnsp->substack, nstk - 3, arg1);
			cowvec_put(&qvi.newnsp->substack, nstk - 2, arg2);
			cowvec_put(&qvi.newnsp->substack, nstk - 1, arg3);
		}
	} else {
		QVecInsert qvi = qvec_insert(&e->epsv, k, nsp);
		if (qvi.newly) {
			nstate_construct_copy(qvi.newnsp, nsp);
		} else {
			int x = nstate_cmp_3(nsp, qvi.newnsp, e->gen, nstk, arg1, arg2, arg3);
			if (x > 0 || (x == 0 && e->minvcnt && cowvec_cmp_from(e->minvoff, &nsp->substack, &qvi.newnsp->substack, e->minvcnt) > 0))
				nstate_copy(qvi.newnsp, nsp);
			else
				return;
		}
		qvi.newnsp->gen = e->gen;
		cowvec_put(&qvi.newnsp->substack, nstk - 3, arg1);
		cowvec_put(&qvi.newnsp->substack, nstk - 2, arg2);
		cowvec_put(&qvi.newnsp->substack, nstk - 1, arg3);
		qset_insert(&e->epsq, k);
	}
}

static bool
is_word_byte(WChar b)
{
	return b == '_' || isalnum(b);
}

static bool
is_word_wide(WChar wc)
{
	return wc == L'_' || iswalnum(wc);
}

static void
execute_epsclosure(Execute *e, QVec *ncsv, WChar wcnext)
{
	const Node *nodes = e->nodes;
	bool (*is_word)(WChar) = e->r->enc == Byte ? is_word_byte : is_word_wide;
	do {
		NInt k = qset_remove(&e->epsq);
		NState *nsp = qvec_lookup(&e->epsv, k);
		if (cowvec_valid(&e->best) && nsp->boff > cowvec_get(&e->best, e->suboff + 0))
			continue;
		const Node *np = &e->nodes[k];
		NInt nstk = np->nstk;
		switch (np->type) {
		case Exit:
			{
				size_t beg = nsp->boff, end = e->off, mincount = e->r->nmin ? cowvec_get(&nsp->substack, 0) : (size_t) -1;
				bool minvalid = e->r->nmin ? cowvec_get(&nsp->substack, e->minvoff) < ((size_t) 1 << (SizeBits - 1)) : false;
				if (!cowvec_valid(&e->best)
				    || beg < cowvec_get(&e->best, e->suboff + 0)
				    || (beg == cowvec_get(&e->best, e->suboff + 0) && end > cowvec_get(&e->best, e->suboff + 1) && (!minvalid || mincount >= e->bestmincount)))
				{
					cowvec_copy(&e->best, &nsp->substack);
					cowvec_put(&e->best, e->suboff + 0, beg);
					cowvec_put(&e->best, e->suboff + 1, end);
					if (minvalid)
						e->bestmincount = MAX(e->bestmincount, mincount);
				}
			}
			break;
		case Fork:
			{
				NInt priority = (NInt) -1;
				do {
					execute_add_1(e, ncsv, k + 1, nstk, nsp, wcnext, priority--);
					k = k + 1 + nodes[k].args[0];
				} while (nodes[k].type != Join);
			}
			break;
		case Goto:
			execute_add(e, ncsv, k + 1 + np->args[1], nstk, nsp, wcnext);
			break;
		case Join:
			execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			break;
		case Loop:
			execute_add_3(e, ncsv, k + 1, nstk, nsp, wcnext, (NInt) e->off, (NInt) -1, (NInt) e->off);
			if (np->args[1])
				execute_add_3(e, ncsv, k + 1 + np->args[0], nstk, nsp, wcnext, (NInt) e->off, (NInt) 0, (NInt) e->off);
			break;
		case MinB:
			{
				size_t w = np->args[0] / SizeBits;
				size_t b = (size_t) 1 << (SizeBits - 1 - np->args[0] % SizeBits);
				NState nscopy;
				nstate_construct_copy(&nscopy, nsp);
				b |= -b;
				size_t x = cowvec_get(&nscopy.substack, e->minvoff + w);
				do {
					if ((x & b) != 0)
						cowvec_put(&nscopy.substack, e->minvoff + w, x & ~b);
					b = -1;
				} while ( w-- > 0);
				execute_add(e, ncsv, k + 1, nstk, &nscopy, wcnext);
				nstate_destruct(&nscopy);
			}
			break;
		case MinL:
			execute_add_1(e, ncsv, k + 1, nstk, nsp, wcnext, e->off);
			break;
		case MinR:
			{
				NState nscopy;
				nstate_construct_copy(&nscopy, nsp);
				size_t mininc = e->off - cowvec_get(&nscopy.substack, np->nstk);
				size_t oldlen = (size_t) -1 - cowvec_get(&nscopy.substack, np->args[0]);
				mininc -= cowvec_get(&nscopy.substack, e->nestoff + np->args[0]);
				cowvec_put(&nscopy.substack, e->nestoff + np->args[0], 0);
				cowvec_put(&nscopy.substack, np->args[0], (size_t) -1 - (oldlen + mininc));
				for (NInt i = np->args[0]; i-- > 0; ) {
					oldlen = (size_t) -1 - cowvec_get(&nscopy.substack, i);
					cowvec_put(&nscopy.substack, i, -1 - (oldlen + mininc));
					cowvec_put(&nscopy.substack, e->nestoff + i, cowvec_get(&nscopy.substack, e->nestoff + i) + mininc);
				}
				execute_add(e, ncsv, k + 1, nstk, &nscopy, wcnext);
				nstate_destruct(&nscopy);
			}
			break;
		case Next:
			execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			if (np->args[1] && e->off > cowvec_get(&nsp->substack, nstk + 3 - 1))
				execute_add_3(e, ncsv, k - np->args[0], nstk + 3, nsp, wcnext, cowvec_get(&nsp->substack, nstk), cowvec_get(&nsp->substack, nstk + 1) - 1, (NInt) e->off);
			break;
		case Skip:
			execute_add_2(e, ncsv, k + 1, nstk, nsp, wcnext, (NInt) e->off, (NInt) 1 ^ np->args[1]);
			execute_add_2(e, ncsv, k + 1 + np->args[0], nstk, nsp, wcnext, (NInt) e->off, (NInt) 0 ^ np->args[1]);
			break;
		case SubL:
			{
				NState nscopy;
				nstate_construct_copy(&nscopy, nsp);
				cowvec_put(&nscopy.substack, nstk - 1, e->off);
				if (np->args[0] != (NInt) -1 && (e->flags & MINRX_REG_NOSUBRESET) == 0)
					for (NInt i = np->args[0] + 1; i <= np->args[1]; ++i) {
						cowvec_put(&nscopy.substack, e->suboff + i * 2, -1);
						cowvec_put(&nscopy.substack, e->suboff + i * 2 + 1, -1);
					}
				execute_add(e, ncsv, k + 1, nstk, &nscopy, wcnext);
				nstate_destruct(&nscopy);
			}
			break;
		case SubR:
			if (np->args[0] != (NInt) -1 && ((e->flags & MINRX_REG_FIRSTSUB) == 0 || cowvec_get(&nsp->substack, e->suboff + np->args[0] * 2) == (NInt) -1)) {
				NState nscopy;
				nstate_construct_copy(&nscopy, nsp);
				cowvec_put(&nscopy.substack, e->suboff + np->args[0] * 2 + 0, cowvec_get(&nsp->substack, nstk));
				cowvec_put(&nscopy.substack, e->suboff + np->args[0] * 2 + 1, e->off);
				execute_add(e, ncsv, k + 1, nstk, &nscopy, wcnext);
				nstate_destruct(&nscopy);
			} else {
				execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			}
			break;
		case ZBOB:
			if (e->off == 0 && (e->flags & MINRX_REG_NOTBOL) == 0)
				execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			break;
		case ZEOB:
			if (wcnext == End && (e->flags & MINRX_REG_NOTEOL) == 0)
				execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			break;
		case ZBOL:
			if (((e->off == 0 && (e->flags & MINRX_REG_NOTBOL) == 0)) || e->wcprev == L'\n')
				execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			break;
		case ZEOL:
			if (((wcnext == End && (e->flags & MINRX_REG_NOTEOL) == 0)) || wcnext == L'\n')
				execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			break;
		case ZBOW:
			if ((e->off == 0 || !is_word(e->wcprev)) && (wcnext != End && is_word(wcnext)))
				execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			break;
		case ZEOW:
			if ((e->off != 0 && is_word(e->wcprev)) && (wcnext == End || !is_word(wcnext)))
				execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			break;
		case ZXOW:
			if (   ((e->off == 0 || !is_word(e->wcprev)) && (wcnext != End && is_word(wcnext)))
			    || ((e->off != 0 && is_word(e->wcprev)) && (wcnext == End || !is_word(wcnext))))
				execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			break;
		case ZNWB:
			if (   (e->off == 0 && wcnext == End)
			    || (e->off == 0 && wcnext != End && !is_word(wcnext))
			    || (e->off != 0 && !is_word(e->wcprev) && wcnext == End)
			    || (e->off != 0 && wcnext != End && is_word(e->wcprev) == is_word(wcnext)))
				execute_add(e, ncsv, k + 1, nstk, nsp, wcnext);
			break;
		default:
			abort();
			break;
		}
	} while (!qset_empty(&e->epsq));
}

static int
execute(Execute *e, size_t nm, minrx_regmatch_t *rm)
{
	QVec mcsvs[2];
	qvec_construct(&mcsvs[0], e->r->nnode);
	qvec_construct(&mcsvs[1], e->r->nnode);
	e->off = wconv_off(&e->wconv);
	WChar wcnext = wconv_nextchr(&e->wconv);
	if ((e->flags & MINRX_REG_RESUME) != 0 && rm && rm[0].rm_eo > 0)
		while (wcnext != End && (ptrdiff_t) e->off < rm[0].rm_eo)
			e->wcprev = wcnext, e->off = wconv_off(&e->wconv), wcnext = wconv_nextchr(&e->wconv);
	NState nsinit;
	nstate_construct(&nsinit, &e->allocator);
	if ((e->flags & MINRX_REG_NOFIRSTBYTES) == 0 && e->r->firstvalid && !cset_test(&*e->r->firstcset, wcnext)) {
	zoom:
		/* empty statement after label */ ;
		const char *cp = e->wconv.cp, *ep = e->wconv.ep;
		if (e->r->firstunique != -1) {
			cp = (const char *) memchr(cp, e->r->firstunique, ep - cp);
			if (cp == (const char *) NULL)
				goto exit;
		} else {
			const bool *fbvec = e->r->firstbytes.vec;
			while (cp != ep && !fbvec[(unsigned char) *cp])
				++cp;
			if (cp == ep)
				goto exit;
		}
		if (cp != e->wconv.cp) {
			if (e->r->enc == UTF8) {
				const char *bp = cp;
				while (bp != e->wconv.cp && cp - bp < 8 && (unsigned char) *--bp >= 0x80)
					;
				e->wconv.cp = (unsigned char) *bp >= 0x80 ? cp - 1 : bp;
			} else {
				e->wconv.cp = cp - 1;
			}
			wcnext = wconv_nextchr(&e->wconv);
		}
		++e->gen, e->wcprev = wcnext, e->off = wconv_off(&e->wconv), wcnext = wconv_nextchr(&e->wconv);
	}
	nsinit.boff = e->off;
	for (size_t i = 0; i < e->r->nmin; ++i)
		cowvec_put(&nsinit.substack, e->nestoff + i, 0);
	execute_add(e, &mcsvs[0], 0, 0, &nsinit, wcnext);
	if (!qset_empty(&e->epsq))
		execute_epsclosure(e, &mcsvs[0], wcnext);
	for (;;) { // unrolled to ping-pong roles of mcsvs[0]/[1]
		if (wcnext == End)
			break;
		++e->gen;
		e->wcprev = wcnext, e->off = wconv_off(&e->wconv), wcnext = wconv_nextchr(&e->wconv);
		while (!qvec_empty(&mcsvs[0])) {
			QVecRemove r = qvec_remove(&mcsvs[0]);
			execute_add(e, &mcsvs[1], r.index + 1, e->nodes[r.index].nstk, &r.nstate, wcnext);
			nstate_destruct(&r.nstate);
		}
		if (!cowvec_valid(&e->best)) {
			nsinit.boff = e->off;
			execute_add(e, &mcsvs[1], 0, 0, &nsinit, wcnext);
		}
		if (!qset_empty(&e->epsq))
			execute_epsclosure(e, &mcsvs[1], wcnext);
		if (qvec_empty(&mcsvs[1])) {
			if (cowvec_valid(&e->best))
				break;
			if ((e->flags & MINRX_REG_NOFIRSTBYTES) == 0 && e->r->firstvalid)
				goto zoom;
		}
		if (wcnext == End)
			break;
		++e->gen;
		e->wcprev = wcnext, e->off = wconv_off(&e->wconv), wcnext = wconv_nextchr(&e->wconv);
		while (!qvec_empty(&mcsvs[1])) {
			QVecRemove r = qvec_remove(&mcsvs[1]);
			execute_add(e, &mcsvs[0], r.index + 1, e->nodes[r.index].nstk, &r.nstate, wcnext);
			nstate_destruct(&r.nstate);
		}
		if (!cowvec_valid(&e->best)) {
			nsinit.boff = e->off;
			execute_add(e, &mcsvs[0], 0, 0, &nsinit, wcnext);
		}
		if (!qset_empty(&e->epsq))
			execute_epsclosure(e, &mcsvs[0], wcnext);
		if (qvec_empty(&mcsvs[0])) {
			if (cowvec_valid(&e->best))
				break;
			if ((e->flags & MINRX_REG_NOFIRSTBYTES) == 0 && e->r->firstvalid)
				goto zoom;
		}
	}
exit:
	nstate_destruct(&nsinit);
	qvec_destruct(&mcsvs[1]);
	qvec_destruct(&mcsvs[0]);
	if (cowvec_valid(&e->best)) {
		if (rm) {
			size_t nsub = MIN(nm, e->r->nsub);
			size_t i;
			for (i = 0; i < nsub; ++i) {
				rm[i].rm_so = cowvec_get(&e->best, e->suboff + i * 2);
				rm[i].rm_eo = cowvec_get(&e->best, e->suboff + i * 2 + 1);
			}
			for (; i < nm; ++i)
				rm[i].rm_so = rm[i].rm_eo = -1;
		}
		return 0;
	} else {
		if (rm)
			for (size_t i = 0; i < nm; ++i)
				rm[i].rm_so = rm[i].rm_eo = -1;
		return MINRX_REG_NOMATCH;
	}
}

int
minrx_regcomp(minrx_regex_t *rx, const char *s, int flags)
{
	return minrx_regncomp(rx, strlen(s), s, flags);
}

int
minrx_regexec(minrx_regex_t *rx, const char *s, size_t nm, minrx_regmatch_t *rm, int flags)
{
	return minrx_regnexec(rx, strlen(s), s, nm, rm, flags);
}

int
minrx_regncomp(minrx_regex_t *rx, size_t ns, const char *s, int flags)
{
	WConv_Encoding enc = MBtoWC;
	char *loc = setlocale(LC_CTYPE, (const char *) NULL);
	if ((strcmp(loc, "C") == 0 || strcmp(loc, "POSIX") == 0 ||
		(flags & MINRX_REG_NATIVE1B) != 0) && MB_CUR_MAX == 1)
		enc = Byte;
	else if (strcmp(nl_langinfo(CODESET), "UTF-8") == 0)
		enc = UTF8;
	Compile c;
	c.flags = (minrx_regcomp_flags_t) flags;
	c.enc = enc;
	wconv_construct(&c.wconv, enc, s, s + ns);
	c.wc = wconv_nextchr(&c.wconv);
	csets_construct(&c.csets);
	c.dot = -1;
	c.esc_s = -1;
	c.esc_S = -1;
	c.esc_w = -1;
	c.esc_W = -1;
	c.nmin = 0;
	c.nsub = 0;
	c.np = (NodePool *) NULL;
	Regexp *r = compile(&c);
	rx->re_regexp = r;
	rx->re_nsub = r->nsub - 1;
	rx->re_compflags = (minrx_regcomp_flags_t) flags;
	return r->err;
}

int
minrx_regnexec(minrx_regex_t *rx, size_t ns, const char *s, size_t nm, minrx_regmatch_t *rm, int flags)
{
	Regexp *r = (Regexp *) rx->re_regexp;
	Execute e;
	execute_construct(&e, r, (minrx_regexec_flags_t) flags, s, s + ns);
	int ret = execute(&e, nm, rm);
	execute_destruct(&e);
	return ret;
}

void
minrx_regfree(minrx_regex_t *rx)
{
	Regexp *r = (Regexp *) rx->re_regexp;
	if (r) {
		if (r->firstcset) {
			cset_destruct(r->firstcset);
			free((void *) r->firstcset);
		}
		free((void *) r->nodes);
		csets_clear(&r->csets);
		free(r);
	}
	rx->re_regexp = (Regexp *) NULL;
}

size_t
minrx_regerror(int errcode, const minrx_regex_t *unused, char *errbuf, size_t errsize)
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
