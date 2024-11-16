#ifndef ALEXER_H_
#define ALEXER_H_

#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#define ALEXER_ARRAY_LEN(xs) (sizeof(xs)/(sizeof((xs)[0])))
#define alexer_return_defer(value) do { result = (value); goto defer; } while(0)

typedef struct {
    char *items;
    size_t count;
    size_t capacity;
} Alexer_String_Builder;

#define ALEXER_ASSERT assert
#define ALEXER_REALLOC realloc
#define ALEXER_FREE free

// Initial capacity of a dynamic array
#ifndef ALEXER_DA_INIT_CAP
#define ALEXER_DA_INIT_CAP 256
#endif

// Append several items to a dynamic array
#define alexer_da_append_many(da, new_items, new_items_count)                                   \
    do {                                                                                        \
        if ((da)->count + (new_items_count) > (da)->capacity) {                                 \
            if ((da)->capacity == 0) {                                                          \
                (da)->capacity = ALEXER_DA_INIT_CAP;                                            \
            }                                                                                   \
            while ((da)->count + (new_items_count) > (da)->capacity) {                          \
                (da)->capacity *= 2;                                                            \
            }                                                                                   \
            (da)->items = ALEXER_REALLOC((da)->items, (da)->capacity*sizeof(*(da)->items));     \
            ALEXER_ASSERT((da)->items != NULL && "Buy more RAM lol");                           \
        }                                                                                       \
        memcpy((da)->items + (da)->count, (new_items), (new_items_count)*sizeof(*(da)->items)); \
        (da)->count += (new_items_count);                                                       \
    } while (0)

// Append a NULL-terminated string to a string builder
#define alexer_sb_append_cstr(sb, cstr)  \
    do {                                 \
        const char *s = (cstr);          \
        size_t n = strlen(s);            \
        alexer_da_append_many(sb, s, n); \
    } while (0)

// Append a single NULL character at the end of a string builder. So then you can
// use it a NULL-terminated C string
#define alexer_sb_append_null(sb) alexer_da_append_many(sb, "", 1)

// TODO: support for utf-8

typedef struct {
    const char *file_path;
    size_t row;
    size_t col;
} Alexer_Loc;

#define Alexer_Loc_Fmt "%s:%zu:%zu"
#define Alexer_Loc_Arg(loc) (loc).file_path, (loc).row, (loc).col

#define ALEXER_LOW32(x) (((uint64_t)(x))&0xFFFFFFFF)
#define ALEXER_ID(kind, index) ((ALEXER_LOW32(index)<<32)|(ALEXER_LOW32(kind)))
#define ALEXER_KIND(id) ALEXER_LOW32(id)
#define ALEXER_INDEX(id) ALEXER_LOW32((id)>>32)

typedef enum {
    ALEXER_INVALID,
    ALEXER_END,
    ALEXER_INT,
    ALEXER_SYMBOL,
    ALEXER_KEYWORD,
    ALEXER_PUNCT,
    // TODO: add support for strings
    ALEXER_STRING,
    ALEXER_COUNT_KINDS,
} Alexer_Kind;

static_assert(ALEXER_COUNT_KINDS == 7, "Amount of kinds have changed");
const char *alexer_kind_names[ALEXER_COUNT_KINDS] = {
    [ALEXER_INVALID] = "INVALID",
    [ALEXER_END]     = "END",
    [ALEXER_INT]     = "INT",
    [ALEXER_SYMBOL]  = "SYMBOL",
    [ALEXER_KEYWORD] = "KEYWORD",
    [ALEXER_PUNCT]   = "PUNCT",
    [ALEXER_STRING]  = "STRING",
};
#define alexer_kind_name(kind) (ALEXER_ASSERT((uint64_t)kind < ALEXER_COUNT_KINDS), alexer_kind_names[(uint64_t)kind])

typedef struct {
    uint64_t id;
    Alexer_Loc loc;
    const char *begin;
    const char *end;
    long int_value;
} Alexer_Token;

bool alexer_token_text_equal(Alexer_Token a, Alexer_Token b);
bool alexer_token_text_equal_cstr(Alexer_Token a, const char *b);

#define Alexer_Token_Fmt "%.*s"
#define Alexer_Token_Arg(t) (int)((t).end - (t).begin), (t).begin

typedef struct {
    const char *opening;
    const char *closing;
} Alexer_ML_Comments;

typedef struct {
    size_t cur;
    size_t bol;
    size_t row;
} Alexer_State;

typedef struct {
    const char *file_path;
    const char *content;
    size_t size;

    size_t cur;
    size_t bol;
    size_t row;

    // TODO: Document properly which order puncts should be in.
    // If one of the puncts is a prefix of another one, the longer one should come first.
    // Maybe we can sort them for the user like that automatically somehow?
    const char **puncts;
    size_t puncts_count;
    const char **keywords;
    size_t keywords_count;
    const char **sl_comments;
    size_t sl_comments_count;
    Alexer_ML_Comments *ml_comments;
    size_t ml_comments_count;
    void (*diagf)(Alexer_Loc loc, const char *level, const char *fmt, ...);
} Alexer;

// TODO: Implement peek?
// I'm not sure about this one. It adds complexity, but you can kind of live without it
// if you just pre-tokenizer everything into a dynamic array and just parse that instead.
// And that gives you longer look-ahead than just being able to peek into the next token.

Alexer alexer_create(const char *file_path, const char *content, size_t size);
bool alexer_get_token(Alexer *l, Alexer_Token *t);
Alexer_State alexer_save(Alexer *l);
void alexer_rewind(Alexer *l, Alexer_State s);
bool alexer_chop_char(Alexer *l);
void alexer_chop_chars(Alexer *l, size_t n);
void alexer_trim_left_ws(Alexer *l);
void alexer_drop_until_endline(Alexer *l);
Alexer_Loc alexer_loc(Alexer *l);
bool alexer_is_symbol(char x); // TODO: Configurable alexer_is_symbol()
bool alexer_is_symbol_start(char x); // TODO: Configurable alexer_is_symbol_start()
// alexer_get_token()
//   Gets the next token. Returns false on END or INVALID. Returns true on any other kind of token.
void alexer_default_diagf(Alexer_Loc loc, const char *level, const char *fmt, ...);
void alexer_ignore_diagf(Alexer_Loc loc, const char *level, const char *fmt, ...);
bool alexer_expect_id(Alexer *l, Alexer_Token t, uint64_t id);
bool alexer_expect_one_of_ids(Alexer *l, Alexer_Token t, uint64_t *ids, size_t ids_count);

#endif // ALEXER_H_

#ifdef ALEXER_IMPLEMENTATION

Alexer alexer_create(const char *file_path, const char *content, size_t size)
{
    return (Alexer) {
        .file_path = file_path,
        .content = content,
        .size = size,
        .diagf = alexer_default_diagf,
    };
}

bool alexer_chop_char(Alexer *l)
{
    if (l->cur < l->size) {
        char x = l->content[l->cur];
        l->cur++;
        if (x == '\n') {
            l->bol = l->cur;
            l->row += 1;
        }
        return true;
    }
    return false;
}

void alexer_chop_chars(Alexer *l, size_t n)
{
    while (n --> 0 && alexer_chop_char(l));
}

void alexer_trim_left_ws(Alexer *l)
{
    // TODO: configurable isspace()
    while (l->cur < l->size && isspace(l->content[l->cur])) {
        alexer_chop_char(l);
    }
}

Alexer_Loc alexer_loc(Alexer *l)
{
    return (Alexer_Loc) {
        .file_path = l->file_path,
        .row = l->row + 1,
        .col = l->cur - l->bol + 1,
    };
}

bool alexer_is_symbol(char x)
{
    return isalnum(x) || x == '_';
}

bool alexer_is_symbol_start(char x)
{
    return isalpha(x) || x == '_';
}

bool alexer_starts_with_cstr(Alexer *l, const char *prefix)
{
    for (size_t i = 0; l->cur + i < l->size && prefix[i] != '\0'; ++i) {
        if (l->content[l->cur + i] != prefix[i]) {
            return false;
        }
    }
    return true;
}

void alexer_drop_until_endline(Alexer *l)
{
    while (l->cur < l->size) {
        char x = l->content[l->cur];
        alexer_chop_char(l);
        if (x == '\n') break;
    }
}

// TODO: multiline comments are not nestable
void alexer_chop_until_prefix(Alexer *l, const char *prefix)
{
    while (l->cur < l->size && !alexer_starts_with_cstr(l, prefix)) {
        alexer_chop_char(l);
    }
}

bool alexer_get_token(Alexer *l, Alexer_Token *t)
{
another_trim_round:
    while (l->cur < l->size) {
        alexer_trim_left_ws(l);

        for (size_t i = 0; i < l->sl_comments_count; ++i) {
            if (alexer_starts_with_cstr(l, l->sl_comments[i])) {
                alexer_drop_until_endline(l);
                goto another_trim_round;
            }
        }

        for (size_t i = 0; i < l->ml_comments_count; ++i) {
            const char *opening = l->ml_comments[i].opening;
            const char *closing = l->ml_comments[i].closing;
            if (alexer_starts_with_cstr(l, opening)) {
                alexer_chop_chars(l, strlen(opening));
                alexer_chop_until_prefix(l, closing);
                alexer_chop_chars(l, strlen(closing));
                goto another_trim_round;
            }
        }

        break; // trimmed everything we could
    }

    memset(t, 0, sizeof(*t));
    t->loc = alexer_loc(l);
    t->begin = &l->content[l->cur];
    t->end = &l->content[l->cur];

    if (l->cur >= l->size) {
        t->id = ALEXER_END;
        return false;
    }

    // Puncts
    for (size_t i = 0; i < l->puncts_count; ++i) {
        if (alexer_starts_with_cstr(l, l->puncts[i])) {
            size_t n = strlen(l->puncts[i]);
            t->id = ALEXER_ID(ALEXER_PUNCT, i);
            t->end += n;
            alexer_chop_chars(l, n);
            return true;
        }
    }

    // Int
    if (isdigit(l->content[l->cur])) {
        t->id = ALEXER_INT;
        while (l->cur < l->size && isdigit(l->content[l->cur])) {
            t->int_value = t->int_value*10 + l->content[l->cur] - '0';
            t->end += 1;
            alexer_chop_char(l);
        }
        return true;
    }

    // Symbol
    if (alexer_is_symbol_start(l->content[l->cur])) {
        t->id = ALEXER_SYMBOL;
        while (l->cur < l->size && alexer_is_symbol(l->content[l->cur])) {
            t->end += 1;
            alexer_chop_char(l);
        }

        // Keyword
        for (size_t i = 0; i < l->keywords_count; ++i) {
            size_t n = strlen(l->keywords[i]);
            if (n == (size_t)(t->end - t->begin) && memcmp(l->keywords[i], t->begin, n) == 0) {
                t->id = ALEXER_ID(ALEXER_KEYWORD, i);
                break;
            }
        }

        return true;
    }

    alexer_chop_char(l);
    t->end += 1;
    return false;
}

void alexer_sb_append_id_display(Alexer_String_Builder *sb, Alexer *l, uint64_t id)
{
    uint64_t kind = ALEXER_KIND(id);
    uint64_t index = ALEXER_INDEX(id);
    switch (kind) {
        case ALEXER_INVALID:
        case ALEXER_END:
        case ALEXER_STRING:
        case ALEXER_INT:
        case ALEXER_SYMBOL:
            alexer_sb_append_cstr(sb, alexer_kind_name(kind));
            break;
        case ALEXER_KEYWORD:
            alexer_sb_append_cstr(sb, alexer_kind_name(kind));
            alexer_sb_append_cstr(sb, " `");
            alexer_sb_append_cstr(sb, (ALEXER_ASSERT(index < l->keywords_count), l->keywords[index]));
            alexer_sb_append_cstr(sb, "`");
            break;
        case ALEXER_PUNCT:
            alexer_sb_append_cstr(sb, alexer_kind_name(kind));
            alexer_sb_append_cstr(sb, " `");
            alexer_sb_append_cstr(sb, (ALEXER_ASSERT(index < l->puncts_count), l->puncts[index]));
            alexer_sb_append_cstr(sb, "`");
            break;
        case ALEXER_COUNT_KINDS:
        default: ALEXER_ASSERT(0 && "unreachable");
    }
}

void alexer_sb_append_token_display(Alexer_String_Builder *sb, Alexer *l, Alexer_Token t)
{
    uint64_t kind = ALEXER_KIND(t.id);
    uint64_t index = ALEXER_INDEX(t.id);
    switch (kind) {
        case ALEXER_INVALID:
        case ALEXER_END:
        case ALEXER_STRING:
        case ALEXER_INT:
        case ALEXER_SYMBOL:
            alexer_sb_append_cstr(sb, alexer_kind_name(kind));
            alexer_sb_append_cstr(sb, " `");
            alexer_da_append_many(sb, t.begin, t.end - t.begin);
            alexer_sb_append_cstr(sb, "`");
            break;
        case ALEXER_KEYWORD:
            alexer_sb_append_cstr(sb, alexer_kind_name(kind));
            alexer_sb_append_cstr(sb, " `");
            alexer_sb_append_cstr(sb, (ALEXER_ASSERT(index < l->keywords_count), l->keywords[index]));
            alexer_sb_append_cstr(sb, "`");
            break;
        case ALEXER_PUNCT:
            alexer_sb_append_cstr(sb, alexer_kind_name(kind));
            alexer_sb_append_cstr(sb, " `");
            alexer_sb_append_cstr(sb, (ALEXER_ASSERT(index < l->puncts_count), l->puncts[index]));
            alexer_sb_append_cstr(sb, "`");
            break;
        case ALEXER_COUNT_KINDS:
        default: ALEXER_ASSERT(0 && "unreachable");
    }
}

bool alexer_expect_id(Alexer *l, Alexer_Token t, uint64_t id)
{
    return alexer_expect_one_of_ids(l, t, &id, 1);
}

bool alexer_expect_one_of_ids(Alexer *l, Alexer_Token t, uint64_t *ids, size_t ids_count)
{
    bool result = false;
    Alexer_String_Builder sb = {0};

    for (size_t i = 0; i < ids_count; ++i) {
        if (t.id == ids[i]) {
            alexer_return_defer(true);
        }
    }

    alexer_sb_append_cstr(&sb, "Expected ");
    for (size_t i = 0; i < ids_count; ++i) {
        if (i > 0) alexer_sb_append_cstr(&sb, ", ");
        alexer_sb_append_id_display(&sb, l, ids[i]);
    }
    alexer_sb_append_cstr(&sb, " but got ");
    alexer_sb_append_token_display(&sb, l, t);
    alexer_sb_append_null(&sb);

    l->diagf(t.loc, "ERROR", "%s", sb.items);

defer:
    free(sb.items);
    return result;
}

void alexer_default_diagf(Alexer_Loc loc, const char *level, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, Alexer_Loc_Fmt": %s: ", Alexer_Loc_Arg(loc), level);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    va_end(args);
}

void alexer_ignore_diagf(Alexer_Loc loc, const char *level, const char *fmt, ...)
{
    (void) loc;
    (void) level;
    (void) fmt;
}

bool alexer_token_text_equal(Alexer_Token a, Alexer_Token b)
{
    size_t na = a.end - a.begin;
    size_t nb = b.end - b.begin;
    if (na != nb) return false;
    return memcmp(a.begin, b.begin, na) == 0;
}

bool alexer_token_text_equal_cstr(Alexer_Token a, const char *b)
{
    size_t na = a.end - a.begin;
    size_t nb = strlen(b);
    if (na != nb) return false;
    return memcmp(a.begin, b, na) == 0;
}

Alexer_State alexer_save(Alexer *l)
{
    return (Alexer_State) {
        .cur = l->cur,
        .bol = l->bol,
        .row = l->row,
    };
}

void alexer_rewind(Alexer *l, Alexer_State s)
{
    l->cur = s.cur;
    l->bol = s.bol;
    l->row = s.row;
}

#endif // ALEXER_IMPLEMENTATION
