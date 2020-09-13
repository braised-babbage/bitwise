#pragma once

#include <stddef.h>
#include <stdlib.h>

#define MAX(x,y) ((x) >= (y) ? (x) : (y))

void *xrealloc(void *ptr, size_t num_bytes); 
void *xmalloc(size_t num_bytes); 
void *xcalloc(size_t num_items, size_t item_size);
void fatal(const char *fmt, ...);
void syntax_error(const char *fmt, ...); 

typedef struct BufHdr {
    size_t len;
    size_t cap;
    char buf[0];
} BufHdr;

#define BUF(x) x
#define buf__hdr(b) ((BufHdr *)((char *)b - offsetof(BufHdr, buf)))
#define buf__fits(b, n) (buf_len(b) + (n) <= buf_cap(b))
#define buf__fit(b, n) ((buf__fits(b, n)) ? 0 : ((b) = (buf__grow(b, buf_len(b) + (n), sizeof(*(b))))))

#define buf_len(b) ((b) ? (buf__hdr(b)->len) : 0U)
#define buf_cap(b) ((b) ? (buf__hdr(b)->cap) : 0U)
#define buf_end(b) ((b) + buf_len(b))
#define buf_push(b, x) (buf__fit(b, 1), (b)[buf_len(b)] = (x), buf__hdr(b)->len++)
#define buf_free(b) ((b) ? (free(buf__hdr(b)), (b) = NULL) : NULL)

void* buf__grow(const void *buf, size_t new_len, size_t elem_size); 

typedef struct Intern {
    size_t len;
    const char *str;
} Intern;

const char *str_intern_range(const char *start, const char *end);
const char *str_intern(const char *str); 