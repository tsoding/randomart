#include <stdio.h>
#include <stdint.h>
#include <time.h>

#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "nob.h"
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
#define ARENA_IMPLEMENTATION
#include "arena.h"

#define WIDTH 800
#define HEIGHT 800

static Arena static_arena = {0};

typedef enum {
    NK_X,
    NK_Y,
    NK_RANDOM,
    NK_RULE,
    NK_NUMBER,
    NK_BOOLEAN,
    NK_ADD,
    NK_MULT,
    NK_MOD,
    NK_GT,
    NK_TRIPLE,
    NK_IF,

    COUNT_NK,
} Node_Kind;

static_assert(COUNT_NK == 12, "Amount of nodes have changed");
const char *nk_names[COUNT_NK] = {
    [NK_X]       = "x",
    [NK_Y]       = "y",
    [NK_RULE]    = "rule",
    [NK_RANDOM]  = "random",
    [NK_NUMBER]  = "number",
    [NK_ADD]     = "add",
    [NK_MULT]    = "mult",
    [NK_MOD]     = "mod",
    [NK_BOOLEAN] = "boolean",
    [NK_GT]      = "gt",
    [NK_TRIPLE]  = "triple",
    [NK_IF]      = "if",
};

typedef struct Node Node;

typedef struct {
    Node *lhs;
    Node *rhs;
} Node_Binop;

typedef struct {
    Node *first;
    Node *second;
    Node *third;
} Node_Triple;

typedef struct {
    Node *cond;
    Node *then;
    Node *elze;
} Node_If;

typedef union {
    float number;
    bool boolean;
    Node_Binop binop;
    Node_Triple triple;
    Node_If iff;
    int rule;
} Node_As;

struct Node {
    Node_Kind kind;
    const char *file;
    int line;
    Node_As as;
};

Node *node_loc(const char *file, int line, Arena *arena, Node_Kind kind)
{
    Node *node = arena_alloc(arena, sizeof(Node));
    node->kind = kind;
    node->file = file;
    node->line = line;
    return node;
}

Node *node_binop_loc(const char *file, int line, Arena *arena, Node_Kind kind, Node *lhs, Node *rhs)
{
    Node *node = node_loc(file, line, arena, kind);
    node->as.binop.lhs = lhs;
    node->as.binop.rhs = rhs;
    return node;
}

Node *node_number_loc(const char *file, int line, Arena *arena, float number)
{
    Node *node = node_loc(file, line, arena, NK_NUMBER);
    node->as.number = number;
    return node;
}
#define node_number(arena, number) node_number_loc(__FILE__, __LINE__, arena, number)
#define node_number_inline(value) (Node){ .kind = NK_NUMBER, .file = __FILE__, .line = __LINE__, .as.number = (value) }

Node *node_rule_loc(const char *file, int line, Arena *arena, int rule)
{
    Node *node = node_loc(file, line, arena, NK_RULE);
    node->as.rule = rule;
    return node;
}
#define node_rule(arena, rule) node_rule_loc(__FILE__, __LINE__, arena, rule)

Node *node_boolean_loc(const char *file, int line, Arena *arena, bool boolean)
{
    Node *node = node_loc(file, line, arena, NK_BOOLEAN);
    node->as.boolean = boolean;
    return node;
}
#define node_boolean(arena, boolean) node_boolean_loc(__FILE__, __LINE__, arena, boolean)
#define node_boolean_inline(value) (Node){ .kind = NK_BOOLEAN, .file = __FILE__, .line = __LINE__, .as.boolean = (value) }

#define node_x(arena)      node_loc(__FILE__, __LINE__, arena, NK_X)
#define node_y(arena)      node_loc(__FILE__, __LINE__, arena, NK_Y)
#define node_random(arena) node_loc(__FILE__, __LINE__, arena, NK_RANDOM)

#define node_add(arena, lhs, rhs)  node_binop_loc(__FILE__, __LINE__, arena, NK_ADD, lhs, rhs)
#define node_mult(arena, lhs, rhs) node_binop_loc(__FILE__, __LINE__, arena, NK_MULT, lhs, rhs)
#define node_mod(arena, lhs, rhs)  node_binop_loc(__FILE__, __LINE__, arena, NK_MOD, lhs, rhs)
#define node_gt(arena, lhs, rhs)   node_binop_loc(__FILE__, __LINE__, arena, NK_GT, lhs, rhs)

Node *node_triple_loc(const char *file, int line, Arena *arena, Node *first, Node *second, Node *third)
{
    Node *node = node_loc(file, line, arena, NK_TRIPLE);
    node->as.triple.first  = first;
    node->as.triple.second = second;
    node->as.triple.third  = third;
    return node;
}
#define node_triple(arena, first, second, third) node_triple_loc(__FILE__, __LINE__, arena, first, second, third)

Node *node_if_loc(const char *file, int line, Arena *arena, Node *cond, Node *then, Node *elze)
{
    Node *node = node_loc(file, line, arena, NK_IF);
    node->as.iff.cond = cond;
    node->as.iff.then = then;
    node->as.iff.elze = elze;
    return node;
}
#define node_if(arena, cond, then, elze) node_if_loc(__FILE__, __LINE__, arena, cond, then, elze)

void node_print(Node *node)
{
    switch (node->kind) {
    case NK_X:
        printf("x");
        break;
    case NK_Y:
        printf("y");
        break;
    case NK_NUMBER:
        printf("%f", node->as.number);
        break;
    case NK_ADD:
        printf("add(");
        node_print(node->as.binop.lhs);
        printf(", ");
        node_print(node->as.binop.rhs);
        printf(")");
        break;
    case NK_MULT:
        printf("mult(");
        node_print(node->as.binop.lhs);
        printf(", ");
        node_print(node->as.binop.rhs);
        printf(")");
        break;
    case NK_MOD:
        printf("mod(");
        node_print(node->as.binop.lhs);
        printf(", ");
        node_print(node->as.binop.rhs);
        printf(")");
        break;
    case NK_BOOLEAN:
        printf("%s", node->as.boolean ? "true" : "false");
        break;
    case NK_GT:
        printf("gt(");
        node_print(node->as.binop.lhs);
        printf(", ");
        node_print(node->as.binop.rhs);
        printf(")");
        break;
    case NK_TRIPLE:
        printf("(");
        node_print(node->as.triple.first);
        printf(", ");
        node_print(node->as.triple.second);
        printf(", ");
        node_print(node->as.triple.third);
        printf(")");
        break;
    case NK_IF:
        printf("if ");
        node_print(node->as.iff.cond);
        printf(" then ");
        node_print(node->as.iff.then);
        printf(" else ");
        node_print(node->as.iff.elze);
        break;
    case NK_RULE:
        printf("rule(%d)", node->as.rule);
        break;
    case NK_RANDOM:
        printf("random");
        break;
    case COUNT_NK:
    default: UNREACHABLE("node_print");
    }
}

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
} RGBA32;

static RGBA32 pixels[WIDTH*HEIGHT];

typedef struct {
    float x, y;
} Vector2;

typedef struct {
    float r, g, b;
} Color;

Color gray_gradient(float x, float y)
{
    UNUSED(y);
    return (Color) {x, x, x};
}

Color cool(float x, float y)
{
    if (x*y >= 0) return (Color){x, y, 1};
    float r = fmodf(x, y);
    return (Color){r, r, r};
}

bool expect_number(Node *expr)
{
    if (expr->kind != NK_NUMBER) {
        printf("%s:%d: ERROR: expected number\n", expr->file, expr->line);
        return false;
    }
    return true;
}

bool expect_boolean(Node *expr)
{
    if (expr->kind != NK_BOOLEAN) {
        printf("%s:%d: ERROR: expected boolean\n", expr->file, expr->line);
        return false;
    }
    return true;
}

bool expect_triple(Node *expr)
{
    if (expr->kind != NK_TRIPLE) {
        printf("%s:%d: ERROR: expected triple\n", expr->file, expr->line);
        return false;
    }
    return true;
}

Node *eval(Node *expr, Arena *arena, float x, float y)
{
    switch (expr->kind) {
    case NK_X:      return node_number_loc(expr->file, expr->line, arena, x);
    case NK_Y:      return node_number_loc(expr->file, expr->line, arena, y);
    case NK_BOOLEAN:
    case NK_NUMBER: return expr;
    case NK_RANDOM:
    case NK_RULE: {
        printf("%s:%d: ERROR: cannot evaluate a node that valid only for grammar definitions\n", expr->file, expr->line);
        return NULL;
    }
    case NK_ADD: {
        Node *lhs = eval(expr->as.binop.lhs, arena, x, y);
        if (!lhs) return NULL;
        if (!expect_number(lhs)) return NULL;
        Node *rhs = eval(expr->as.binop.rhs, arena, x, y);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_number_loc(expr->file, expr->line, arena, lhs->as.number + rhs->as.number);
    }
    case NK_MULT: {
        Node *lhs = eval(expr->as.binop.lhs, arena, x, y);
        if (!lhs) return NULL;
        if (!expect_number(lhs)) return NULL;
        Node *rhs = eval(expr->as.binop.rhs, arena, x, y);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_number_loc(expr->file, expr->line, arena, lhs->as.number * rhs->as.number);
    }
    case NK_MOD: {
        Node *lhs = eval(expr->as.binop.lhs, arena, x, y);
        if (!lhs) return NULL;
        if (!expect_number(lhs)) return NULL;
        Node *rhs = eval(expr->as.binop.rhs, arena, x, y);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_number_loc(expr->file, expr->line, arena, fmodf(lhs->as.number, rhs->as.number));
    }
    case NK_GT: {
        Node *lhs = eval(expr->as.binop.lhs, arena, x, y);
        if (!lhs) return NULL;
        if (!expect_number(lhs)) return NULL;
        Node *rhs = eval(expr->as.binop.rhs, arena, x, y);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_boolean_loc(expr->file, expr->line, arena, lhs->as.number > rhs->as.number);
    }
    case NK_TRIPLE: {
        Node *first = eval(expr->as.triple.first, arena, x, y);
        if (!first) return NULL;
        Node *second = eval(expr->as.triple.second, arena, x, y);
        if (!second) return NULL;
        Node *third = eval(expr->as.triple.third, arena, x, y);
        if (!third) return NULL;
        return node_triple_loc(expr->file, expr->line, arena, first, second, third);
    }
    case NK_IF: {
        Node *cond = eval(expr->as.iff.cond, arena, x, y);
        if (!cond) return NULL;
        if (!expect_boolean(cond)) return NULL;
        Node *then = eval(expr->as.iff.then, arena, x, y);
        if (!then) return NULL;
        Node *elze = eval(expr->as.iff.elze, arena, x, y);
        if (!elze) return NULL;
        return cond->as.boolean ? then : elze;
    }
    case COUNT_NK:
    default: UNREACHABLE("eval");
    }
}

bool eval_func(Node *f, Arena *arena, float x, float y, Color *c)
{
    Node *result = eval(f, arena, x, y);
    if (!result) return false;
    if (!expect_triple(result)) return false;
    if (!expect_number(result->as.triple.first)) return false;
    if (!expect_number(result->as.triple.second)) return false;
    if (!expect_number(result->as.triple.third)) return false;
    c->r = result->as.triple.first->as.number;
    c->g = result->as.triple.second->as.number;
    c->b = result->as.triple.third->as.number;
    return true;
}

bool render_pixels(Node *f)
{
    Arena arena = {0};
    for (size_t y = 0; y < HEIGHT; ++y) {
        float ny = (float)y/HEIGHT*2.0f - 1;
        for (size_t x = 0; x < WIDTH; ++x) {
            float nx = (float)x/WIDTH*2.0f - 1;
            Color c;
            if (!eval_func(f, &arena, nx, ny, &c)) return false;
            arena_reset(&arena);
            size_t index = y*WIDTH + x;
            pixels[index].r = (c.r + 1)/2*255;
            pixels[index].g = (c.g + 1)/2*255;
            pixels[index].b = (c.b + 1)/2*255;
            pixels[index].a = 255;
        }
    }
    return true;
}

#define node_print_ln(node) (node_print(node), printf("\n"))

typedef struct {
    Node *node;
    float probability;
} Grammar_Branch;

typedef struct {
    Grammar_Branch *items;
    size_t capacity;
    size_t count;
} Grammar_Branches;

typedef struct {
    Grammar_Branches *items;
    size_t capacity;
    size_t count;
} Grammar;

void grammar_print(Grammar grammar)
{
    for (size_t i = 0; i < grammar.count; ++i) {
        printf("%zu ::= ", i);
        Grammar_Branches *branches = &grammar.items[i];
        for (size_t j = 0; j < branches->count; ++j) {
            if (j > 0) printf(" | ");
            node_print(branches->items[j].node);
            printf(" [%.02f]", branches->items[j].probability);
        }
        printf("\n");
    }
}

Node *gen_rule(Grammar grammar, Arena *arena, size_t rule, int depth);

float rand_float(void)
{
    return (float) rand() / (float) RAND_MAX;
}

Node *gen_node(Grammar grammar, Arena *arena, Node *node, int depth)
{
    switch (node->kind) {
    case NK_X:
    case NK_Y:
    case NK_NUMBER:
    case NK_BOOLEAN:
        return node;

    case NK_ADD:
    case NK_MULT:
    case NK_MOD:
    case NK_GT: {
        Node *lhs = gen_node(grammar, arena, node->as.binop.lhs, depth);
        if (!lhs) return NULL;
        Node *rhs = gen_node(grammar, arena, node->as.binop.rhs, depth);
        if (!rhs) return NULL;
        return node_binop_loc(node->file, node->line, arena, node->kind, lhs, rhs);
    }

    case NK_TRIPLE: {
        Node *first  = gen_node(grammar, arena, node->as.triple.first, depth);
        if (!first) return NULL;
        Node *second = gen_node(grammar, arena, node->as.triple.second, depth);
        if (!second) return NULL;
        Node *third  = gen_node(grammar, arena, node->as.triple.third, depth);
        if (!third) return NULL;
        return node_triple_loc(node->file, node->line, arena, first, second, third);
    }
    case NK_IF: {
        Node *cond = gen_node(grammar, arena, node->as.iff.cond, depth);
        if (!cond) return NULL;
        Node *then = gen_node(grammar, arena, node->as.iff.then, depth);
        if (!then) return NULL;
        Node *elze = gen_node(grammar, arena, node->as.iff.elze, depth);
        if (!elze) return NULL;
        return node_if_loc(node->file, node->line, arena, cond, then, elze);
    }

    case NK_RULE:
        return gen_rule(grammar, arena, node->as.rule, depth - 1);

    case NK_RANDOM:
        return node_number_loc(node->file, node->line, arena, rand_float()*2.0f - 1.0f);

    case COUNT_NK:
    default:
        UNREACHABLE("gen_node");
    }
}

#define GEN_RULE_MAX_ATTEMPTS 100

Node *gen_rule(Grammar grammar, Arena *arena, size_t rule, int depth)
{
    if (depth <= 0) return NULL;

    assert(rule < grammar.count);

    Grammar_Branches *branches = &grammar.items[rule];
    assert(branches->count > 0);

    Node *node = NULL;
    for (size_t attempts = 0; node == NULL && attempts < GEN_RULE_MAX_ATTEMPTS; ++attempts) {
        // [0......][...][...1]
        float p = rand_float();
        float t = 0.0f;
        for (size_t i = 0; i < branches->count; ++i) {
            t += branches->items[i].probability;
            if (t >= p) {
                node = gen_node(grammar, arena, branches->items[i].node, depth - 1);
                break;
            }
        }
    }
    return node;
}

bool get_number(Node *node, float *result) 
{
    if (node->kind != NK_NUMBER)
        return false;
    if (result) 
        *result = node->as.number;
    return true;
}

bool get_boolean(Node *node, bool *result) 
{
    if (node->kind != NK_BOOLEAN)
        return false;
    if (result) 
        *result = node->as.boolean;
    return true;
}

#define OPTIMIZE_MAX_PASSES 100

// TODO: Probably try to free the discarded nodes
bool optimize_expr(Arena *arena, Node *expr) 
{
    switch (expr->kind) {
    case NK_X:
    case NK_Y:
    case NK_BOOLEAN:
    case NK_NUMBER: return false;
    case NK_RANDOM:
    case NK_RULE: {
        printf("%s:%d: ERROR: cannot optimize a node that valid only for grammar definitions\n", expr->file, expr->line);
        return false;
    }
    case NK_ADD: {
        bool res = optimize_expr(arena, expr->as.binop.lhs) || optimize_expr(arena, expr->as.binop.rhs);
        float lhs, rhs;
        if (!(get_number(expr->as.binop.lhs, &lhs) && get_number(expr->as.binop.rhs, &rhs)))
            return res;
        *expr = node_number_inline(lhs+rhs);
        return true;
    }
    case NK_MULT: {
        bool res = optimize_expr(arena, expr->as.binop.lhs) || optimize_expr(arena, expr->as.binop.rhs);
        float lhs, rhs;
        if (!(get_number(expr->as.binop.lhs, &lhs) && get_number(expr->as.binop.rhs, &rhs)))
            return res;
        *expr = node_number_inline(lhs*rhs);
        return true;
    }
    case NK_MOD: {
        bool res = optimize_expr(arena, expr->as.binop.lhs) || optimize_expr(arena, expr->as.binop.rhs);
        float lhs, rhs;
        if (!(get_number(expr->as.binop.lhs, &lhs) && get_number(expr->as.binop.rhs, &rhs)))
            return res;
        *expr = node_number_inline(fmodf(lhs,rhs));
        return true;
    }
    case NK_GT: {
        bool res = optimize_expr(arena, expr->as.binop.lhs) || optimize_expr(arena, expr->as.binop.rhs);
        float lhs, rhs;
        if (!(get_number(expr->as.binop.lhs, &lhs) && get_number(expr->as.binop.rhs, &rhs)))
            return res;
        *expr = node_boolean_inline(lhs>rhs);
        return true;
    }
    case NK_TRIPLE: {
        return 
            optimize_expr(arena, expr->as.triple.first) ||
            optimize_expr(arena, expr->as.triple.second) ||
            optimize_expr(arena, expr->as.triple.third)
        ;
    }
    case NK_IF: {
        bool res = optimize_expr(arena, expr->as.iff.then) || optimize_expr(arena, expr->as.iff.elze) || optimize_expr(arena, expr->as.iff.cond);
        float then, elze;
        bool cond;
        if (!(get_number(expr->as.iff.then, &then) && get_number(expr->as.iff.elze, &elze) && get_boolean(expr->as.iff.cond, &cond)))
            return res;
        *expr = cond ? node_number_inline(then) : node_number_inline(elze);
        return true;
    }
    case COUNT_NK:
    default: UNREACHABLE("optimize_expr");
    }
}

bool optimize_func(Arena *arena, Node *func) 
{
    for (size_t i = 0; i < OPTIMIZE_MAX_PASSES; i++) {
        if (!optimize_expr(arena, func))
            return true;
    }
    return false;
}

size_t arch[] = {2, 28, 28, 9, 3};

int main()
{
    srand(time(0));
    Grammar grammar = {0};
    Grammar_Branches branches = {0};
    int e = 0;
    int a = 1;
    int c = 2;

    arena_da_append(&static_arena, &branches, ((Grammar_Branch) {
        .node = node_triple(&static_arena, node_rule(&static_arena, c), node_rule(&static_arena, c), node_rule(&static_arena, c)),
        .probability = 1.0f
    }));
    arena_da_append(&static_arena, &grammar, branches);
    memset(&branches, 0, sizeof(branches));

    arena_da_append(&static_arena, &branches, ((Grammar_Branch) {
        .node = node_random(&static_arena),
        .probability = 1.0/3.0,
    }));
    arena_da_append(&static_arena, &branches, ((Grammar_Branch) {
        .node = node_x(&static_arena),
        .probability = 1.0/3.0,
    }));
    arena_da_append(&static_arena, &branches, ((Grammar_Branch) {
        .node = node_y(&static_arena),
        .probability = 1.0/3.0,
    }));
    arena_da_append(&static_arena, &grammar, branches);
    memset(&branches, 0, sizeof(branches));

    arena_da_append(&static_arena, &branches, ((Grammar_Branch) {
        .node = node_rule(&static_arena, a),
        .probability = 1.f/4.f,
        // .probability = 1.f/2.f,
    }));
    arena_da_append(&static_arena, &branches, ((Grammar_Branch) {
        .node = node_add(&static_arena, node_rule(&static_arena, c), node_rule(&static_arena, c)),
        .probability = 3.f/8.f,
        // .probability = 1.f/4.f,
    }));
    arena_da_append(&static_arena, &branches, ((Grammar_Branch) {
        .node = node_mult(&static_arena, node_rule(&static_arena, c), node_rule(&static_arena, c)),
        .probability = 3.f/8.f,
        // .probability = 1.f/4.f,
    }));
    arena_da_append(&static_arena, &grammar, branches);
    memset(&branches, 0, sizeof(branches));

    Node *f = gen_rule(grammar, &static_arena, e, 30);
    if (!f) {
        fprintf(stderr, "ERROR: the crappy generation process could not terminate\n");
        return 1;
    }
    if (!optimize_func(&static_arena, f)) {
        nob_log(WARNING, "Exceeded maximum optimization passes");
    }
    node_print_ln(f);

    // bool ok = render_pixels(node_triple(node_x(), node_x(), node_x()));
    // bool ok = render_pixels(
    //     node_if(
    //         node_gt(node_mult(node_x(), node_y()), node_number(0)),
    //         node_triple(
    //             node_x(),
    //             node_y(),
    //             node_number(1)),
    //         node_triple(
    //             node_mod(node_x(), node_y()),
    //             node_mod(node_x(), node_y()),
    //             node_mod(node_x(), node_y()))));

    bool ok = render_pixels(f);

    if (!ok) return 1;
    const char *output_path = "output.png";
    if (!stbi_write_png(output_path, WIDTH, HEIGHT, 4, pixels, WIDTH*sizeof(RGBA32))) {
        nob_log(ERROR, "Could not save image %s", output_path);
        return 1;
    }
    nob_log(INFO, "Generated %s", output_path);
    return 0;
}
