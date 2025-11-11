#ifndef CHARSET_H
#define CHARSET_H 1

/*
 * Copyright (C) 2023, 2024, 2025, Arnold David Robbins.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _charset charset_t;

// error code values:
enum {
	CSET_SUCCESS = 0,	// No problems
	CSET_EBADPTR,		// NULL pointer received
	CSET_EFROZEN,		// Cannot add more characters to the set
	CSET_ECOLLATE,		// Corresponds to REG_ECOLLATE
	CSET_ECTYPE,		// Corresponds to REG_ECTYPE
	CSET_ESPACE,		// Corresponds to REG_ESPACE
	CSET_ERANGE,		// Corresponds to REG_ERANGE
};
charset_t *charset_create(int *errcode, int mb_cur_max, bool is_utf8);
int charset_add_char(charset_t *set, int32_t wc);
int charset_add_range(charset_t *set, int32_t first, int32_t last);
charset_t *charset_invert(charset_t *set, int *errcode);
int charset_set_no_newlines(charset_t *set, bool no_newlines);
int charset_add_equiv(charset_t *set, int32_t equiv);
int charset_add_collate(charset_t *set, const int32_t *collate);
int charset_add_cclass(charset_t *set, const char *cclass);
charset_t *charset_copy(charset_t *set, int *errcode);
int charset_merge(charset_t *dest, charset_t *src);
bool charset_in_set(const charset_t *set, int32_t the_char);
int charset_free(const charset_t *set);
#define MAX_FIRSTBYTES	256
typedef struct {
	bool bytes[MAX_FIRSTBYTES];
} charset_firstbytes_t;

charset_firstbytes_t charset_firstbytes(charset_t *set, int *errcode);
void charset_dump(const charset_t *set, FILE *fp, bool use_c_format);
int charset_finalize(charset_t *set);

#ifdef __cplusplus
}
#endif
#endif /* CHARSET_H */
