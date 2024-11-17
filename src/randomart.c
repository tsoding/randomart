#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <math.h>

#include <raylib.h>
#include <rlgl.h>

#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "nob.h"
#define ARENA_IMPLEMENTATION
#include "arena.h"
#include "ffmpeg.h"
#define ALEXER_IMPLEMENTATION
#include "alexer.h"

static Arena static_arena = {0};
static Arena *context_arena = &static_arena;
#define context_da_append(da, x) arena_da_append(context_arena, (da), (x))

typedef enum {
    NK_X,
    NK_Y,
    NK_S,
    NK_T,
    NK_RANDOM,
    NK_RULE,
    NK_NUMBER,
    NK_BOOLEAN,
    NK_SQRT,
    NK_ABS,
    NK_SIN,
    NK_ADD,
    NK_MULT,
    NK_MOD,
    NK_GT,
    NK_TRIPLE,
    NK_IF,

    COUNT_NK,
} Node_Kind;

static_assert(COUNT_NK == 17, "Amount of nodes have changed");
const char *nk_names[COUNT_NK] = {
    [NK_X]       = "x",
    [NK_Y]       = "y",
    [NK_S]       = "s",
    [NK_T]       = "t",
    [NK_RULE]    = "rule",
    [NK_RANDOM]  = "random",
    [NK_NUMBER]  = "number",
    [NK_SQRT]    = "sqrt",
    [NK_SIN]    = "sin",
    [NK_ABS]     = "abs",
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
    Node *unop;
    Node_Triple triple;
    Node_If iff;
    Alexer_Token rule;
} Node_As;

struct Node {
    Node_Kind kind;
    const char *file;
    int line;
    Node_As as;
};

Node *node_loc(const char *file, int line, Node_Kind kind)
{
    Node *node = arena_alloc(context_arena, sizeof(Node));
    node->kind = kind;
    node->file = file;
    node->line = line;
    return node;
}

Node *node_unop_loc(const char *file, int line, Node_Kind kind, Node *unop)
{
    Node *node = node_loc(file, line, kind);
    node->as.unop = unop;
    return node;
}

Node *node_binop_loc(const char *file, int line, Node_Kind kind, Node *lhs, Node *rhs)
{
    Node *node = node_loc(file, line, kind);
    node->as.binop.lhs = lhs;
    node->as.binop.rhs = rhs;
    return node;
}

Node *node_number_loc(const char *file, int line, float number)
{
    Node *node = node_loc(file, line, NK_NUMBER);
    node->as.number = number;
    return node;
}
#define node_number(number) node_number_loc(__FILE__, __LINE__, number)

Node *node_rule_loc(const char *file, int line, Alexer_Token rule)
{
    Node *node = node_loc(file, line, NK_RULE);
    node->as.rule = rule;
    return node;
}
#define node_rule(rule) node_rule_loc(__FILE__, __LINE__, rule)

Node *node_boolean_loc(const char *file, int line, bool boolean)
{
    Node *node = node_loc(file, line, NK_BOOLEAN);
    node->as.boolean = boolean;
    return node;
}
#define node_boolean(boolean) node_boolean_loc(__FILE__, __LINE__, boolean)

#define node_x()      node_loc(__FILE__, __LINE__, NK_X)
#define node_y()      node_loc(__FILE__, __LINE__, NK_Y)
#define node_s()      node_loc(__FILE__, __LINE__, NK_S)
#define node_t()      node_loc(__FILE__, __LINE__, NK_T)
#define node_random() node_loc(__FILE__, __LINE__, NK_RANDOM)

#define node_sqrt(unop)  node_unop_loc(__FILE__, __LINE__, NK_SQRT, unop)

#define node_add(lhs, rhs)  node_binop_loc(__FILE__, __LINE__, NK_ADD, lhs, rhs)
#define node_mult(lhs, rhs) node_binop_loc(__FILE__, __LINE__, NK_MULT, lhs, rhs)
#define node_mod(lhs, rhs)  node_binop_loc(__FILE__, __LINE__, NK_MOD, lhs, rhs)
#define node_gt(lhs, rhs)   node_binop_loc(__FILE__, __LINE__, NK_GT, lhs, rhs)

Node *node_triple_loc(const char *file, int line, Node *first, Node *second, Node *third)
{
    Node *node = node_loc(file, line, NK_TRIPLE);
    node->as.triple.first  = first;
    node->as.triple.second = second;
    node->as.triple.third  = third;
    return node;
}
#define node_triple(first, second, third) node_triple_loc(__FILE__, __LINE__, first, second, third)

Node *node_if_loc(const char *file, int line, Node *cond, Node *then, Node *elze)
{
    Node *node = node_loc(file, line, NK_IF);
    node->as.iff.cond = cond;
    node->as.iff.then = then;
    node->as.iff.elze = elze;
    return node;
}
#define node_if(cond, then, elze) node_if_loc(__FILE__, __LINE__, cond, then, elze)

void node_print(Node *node)
{
    switch (node->kind) {
    case NK_X:
        printf("x");
        break;
    case NK_Y:
        printf("y");
        break;
    case NK_S:
        printf("s");
        break;
    case NK_T:
        printf("t");
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
    case NK_SQRT:
        printf("sqrt(");
        node_print(node->as.unop);
        printf(")");
        break;
    case NK_SIN:
        printf("sin(");
        node_print(node->as.unop);
        printf(")");
        break;
    case NK_ABS:
        printf("abs(");
        node_print(node->as.unop);
        printf(")");
        break;
    case NK_RULE:
        printf("rule("Alexer_Token_Fmt")", Alexer_Token_Arg(node->as.rule));
        break;
    case NK_RANDOM:
        printf("random");
        break;
    case COUNT_NK:
    default: UNREACHABLE("node_print");
    }
}

Vector3 gray_gradient(float x, float y)
{
    UNUSED(y);
    return (Vector3) {x, x, x};
}

Vector3 cool(float x, float y)
{
    if (x*y >= 0) return (Vector3){x, y, 1};
    float r = fmodf(x, y);
    return (Vector3){r, r, r};
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

Node *eval(Node *expr, float x, float y, float s, float t)
{
    switch (expr->kind) {
    case NK_X:      return node_number_loc(expr->file, expr->line, x);
    case NK_Y:      return node_number_loc(expr->file, expr->line, y);
    case NK_S:      return node_number_loc(expr->file, expr->line, s);
    case NK_T:      return node_number_loc(expr->file, expr->line, t);
    case NK_BOOLEAN:
    case NK_NUMBER: return expr;
    case NK_RANDOM:
    case NK_RULE: {
        printf("%s:%d: ERROR: cannot evaluate a node that valid only for grammar definitions\n", expr->file, expr->line);
        return NULL;
    }
    case NK_SQRT: {
        Node *rhs = eval(expr->as.unop, x, y, s, t);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_number_loc(expr->file, expr->line, sqrtf(rhs->as.number));
    }
    case NK_SIN: {
        Node *rhs = eval(expr->as.unop, x, y, s, t);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_number_loc(expr->file, expr->line, sinf(rhs->as.number));
    }
    case NK_ABS: {
        Node *rhs = eval(expr->as.unop, x, y, s, t);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_number_loc(expr->file, expr->line, fabsf(rhs->as.number));
    }
    case NK_ADD: {
        Node *lhs = eval(expr->as.binop.lhs, x, y, s, t);
        if (!lhs) return NULL;
        if (!expect_number(lhs)) return NULL;
        Node *rhs = eval(expr->as.binop.rhs, x, y, s, t);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_number_loc(expr->file, expr->line, lhs->as.number + rhs->as.number);
    }
    case NK_MULT: {
        Node *lhs = eval(expr->as.binop.lhs, x, y, s, t);
        if (!lhs) return NULL;
        if (!expect_number(lhs)) return NULL;
        Node *rhs = eval(expr->as.binop.rhs, x, y, s, t);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_number_loc(expr->file, expr->line, lhs->as.number * rhs->as.number);
    }
    case NK_MOD: {
        Node *lhs = eval(expr->as.binop.lhs, x, y, s, t);
        if (!lhs) return NULL;
        if (!expect_number(lhs)) return NULL;
        Node *rhs = eval(expr->as.binop.rhs, x, y, s, t);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_number_loc(expr->file, expr->line, fmodf(lhs->as.number, rhs->as.number));
    }
    case NK_GT: {
        Node *lhs = eval(expr->as.binop.lhs, x, y, s, t);
        if (!lhs) return NULL;
        if (!expect_number(lhs)) return NULL;
        Node *rhs = eval(expr->as.binop.rhs, x, y, s, t);
        if (!rhs) return NULL;
        if (!expect_number(rhs)) return NULL;
        return node_boolean_loc(expr->file, expr->line, lhs->as.number > rhs->as.number);
    }
    case NK_TRIPLE: {
        Node *first = eval(expr->as.triple.first, x, y, s, t);
        if (!first) return NULL;
        Node *second = eval(expr->as.triple.second, x, y, s, t);
        if (!second) return NULL;
        Node *third = eval(expr->as.triple.third, x, y, s, t);
        if (!third) return NULL;
        return node_triple_loc(expr->file, expr->line, first, second, third);
    }
    case NK_IF: {
        Node *cond = eval(expr->as.iff.cond, x, y, s, t);
        if (!cond) return NULL;
        if (!expect_boolean(cond)) return NULL;
        Node *then = eval(expr->as.iff.then, x, y, s, t);
        if (!then) return NULL;
        Node *elze = eval(expr->as.iff.elze, x, y, s, t);
        if (!elze) return NULL;
        return cond->as.boolean ? then : elze;
    }
    case COUNT_NK:
    default: UNREACHABLE("eval");
    }
}

bool eval_func(Node *f, float x, float y, float s, float t, Vector3 *c)
{
    Node *result = eval(f, x, y, s, t);
    if (!result) return false;
    if (!expect_triple(result)) return false;
    if (!expect_number(result->as.triple.first)) return false;
    if (!expect_number(result->as.triple.second)) return false;
    if (!expect_number(result->as.triple.third)) return false;
    c->x = result->as.triple.first->as.number;
    c->y = result->as.triple.second->as.number;
    c->z = result->as.triple.third->as.number;
    return true;
}

float clamp_float(float v, float lo, float hi)
{
    if (v < lo) return lo;
    if (v > hi) return hi;
    return v;
}

bool render_image(Image image, Node *f)
{
    Color *pixels = image.data;
    bool result = true;
    Arena temp_arena = {0};
    Arena *saved_arena = context_arena;
    context_arena = &temp_arena;
    for (int y = 0; y < image.height; ++y) {
        float ny = (float)y/image.height*2.0f - 1;
        for (int x = 0; x < image.width; ++x) {
            float nx = (float)x/image.width*2.0f - 1;
            Vector3 c;
            if (!eval_func(f, nx, ny, 0.0, 0.0, &c)) return_defer(false);
            arena_reset(&temp_arena);
            size_t index = y*image.width + x;
            pixels[index].r = clamp_float((c.x + 1)/2*255, 0, 255);
            pixels[index].g = clamp_float((c.y + 1)/2*255, 0, 255);
            pixels[index].b = clamp_float((c.z + 1)/2*255, 0, 255);
            pixels[index].a = 255;
        }
    }
defer:
    arena_free(&temp_arena);
    context_arena = saved_arena;
    return result;
}

#define node_print_ln(node) (node_print(node), printf("\n"))

typedef struct {
    Node *node;
    size_t weight;
} Grammar_Branch;

typedef struct {
    Grammar_Branch *items;
    size_t capacity;
    size_t count;
    size_t weight_sum;
    Alexer_Token name;
} Grammar_Branches;

typedef struct {
    Grammar_Branches *items;
    size_t capacity;
    size_t count;
} Grammar;

void grammar_print(Grammar grammar)
{
    for (size_t i = 0; i < grammar.count; ++i) {
        Grammar_Branches *branches = &grammar.items[i];
        printf(Alexer_Token_Fmt"\n", Alexer_Token_Arg(branches->name));
        for (size_t j = 0; j < branches->count; ++j) {
            Grammar_Branch *branch = &branches->items[j];
            printf("  ");
            for (size_t k = 0; k < branch->weight; ++k) printf("|");
            node_print_ln(branch->node);
        }
        printf("  ;\n");
    }
}

Node *gen_rule(Grammar grammar, Alexer_Token rule, int depth);

float rand_float(void)
{
    return (float) rand() / (float) RAND_MAX;
}

Node *gen_node(Grammar grammar, Node *node, int depth)
{
    switch (node->kind) {
    case NK_X:
    case NK_Y:
    case NK_S:
    case NK_T:
    case NK_NUMBER:
    case NK_BOOLEAN:
        return node;

    case NK_SQRT: {
        Node *rhs = gen_node(grammar, node->as.unop, depth);
        if (!rhs) return NULL;
        return node_unop_loc(node->file, node->line, node->kind, rhs);
    }
    case NK_SIN: {
        Node *rhs = gen_node(grammar, node->as.unop, depth);
        if (!rhs) return NULL;
        return node_unop_loc(node->file, node->line, node->kind, rhs);
    }
    case NK_ABS: {
        Node *rhs = gen_node(grammar, node->as.unop, depth);
        if (!rhs) return NULL;
        return node_unop_loc(node->file, node->line, node->kind, rhs);
    }

    case NK_ADD:
    case NK_MULT:
    case NK_MOD:
    case NK_GT: {
        Node *lhs = gen_node(grammar, node->as.binop.lhs, depth);
        if (!lhs) return NULL;
        Node *rhs = gen_node(grammar, node->as.binop.rhs, depth);
        if (!rhs) return NULL;
        return node_binop_loc(node->file, node->line, node->kind, lhs, rhs);
    }

    case NK_TRIPLE: {
        Node *first  = gen_node(grammar, node->as.triple.first, depth);
        if (!first) return NULL;
        Node *second = gen_node(grammar, node->as.triple.second, depth);
        if (!second) return NULL;
        Node *third  = gen_node(grammar, node->as.triple.third, depth);
        if (!third) return NULL;
        return node_triple_loc(node->file, node->line, first, second, third);
    }
    case NK_IF: {
        Node *cond = gen_node(grammar, node->as.iff.cond, depth);
        if (!cond) return NULL;
        Node *then = gen_node(grammar, node->as.iff.then, depth);
        if (!then) return NULL;
        Node *elze = gen_node(grammar, node->as.iff.elze, depth);
        if (!elze) return NULL;
        return node_if_loc(node->file, node->line, cond, then, elze);
    }

    case NK_RULE:
        return gen_rule(grammar, node->as.rule, depth - 1);

    case NK_RANDOM:
        return node_number_loc(node->file, node->line, rand_float()*2.0f - 1.0f);

    case COUNT_NK:
    default:
        UNREACHABLE("gen_node");
    }
}

#define GEN_RULE_MAX_ATTEMPTS 100

Grammar_Branches *branches_by_name(Grammar *grammar, Alexer_Token rule)
{
    for (size_t i = 0; i < grammar->count; ++i) {
        if (alexer_token_text_equal(grammar->items[i].name, rule)) {
            return &grammar->items[i];
        }
    }
    alexer_default_diagf(rule.loc, "ERROR", "Rule "Alexer_Token_Fmt" does not exist", Alexer_Token_Arg(rule));
    return NULL;
}

Node *gen_rule(Grammar grammar, Alexer_Token rule, int depth)
{
    if (depth <= 0) return NULL;

    Grammar_Branches *branches = branches_by_name(&grammar, rule);
    if (branches == NULL) return NULL;
    assert(branches->count > 0);

    Node *node = NULL;
    for (size_t attempts = 0; node == NULL && attempts < GEN_RULE_MAX_ATTEMPTS; ++attempts) {
        // [0......][...][...1]
        float p = rand_float();
        float t = 0.0f;
        for (size_t i = 0; i < branches->count; ++i) {
            t += (float)branches->items[i].weight/branches->weight_sum;
            if (t >= p) {
                node = gen_node(grammar, branches->items[i].node, depth - 1);
                break;
            }
        }
    }
    return node;
}

void grammar_append_branches(Grammar *grammar, Grammar_Branches *branches, Alexer_Token name)
{
    branches->name = name;
    branches->weight_sum = 0;
    for (size_t i = 0; i < branches->count; ++i) {
        branches->weight_sum += branches->items[i].weight;
    }
    context_da_append(grammar, *branches);
    memset(branches, 0, sizeof(*branches));
}

#define SYMBOL(name_cstr) symbol_impl(__FILE__, __LINE__, name_cstr)

Alexer_Token symbol_impl(const char *file, int line, const char *name_cstr)
{
    return (Alexer_Token) {
        .id = ALEXER_SYMBOL,
        .loc = {
            .file_path = file,
            .row = line,
            .col = 0,
        },
        .begin = name_cstr,
        .end = name_cstr + strlen(name_cstr),
    };
}

// TODO: load grammar from file
Alexer_Token default_grammar(Grammar *grammar)
{
    Grammar_Branches branches = {0};

    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_triple(node_rule(SYMBOL("c")), node_rule(SYMBOL("c")), node_rule(SYMBOL("c"))),
        .weight = 1,
    }));
    grammar_append_branches(grammar, &branches, SYMBOL("e"));

    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_random(),
        .weight = 1,
    }));
    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_x(),
        .weight = 1,
    }));
    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_y(),
        .weight = 1,
    }));
    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_s(),
        .weight = 1,
    }));
    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_t(),
        .weight = 1,
    }));
    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_sqrt(
            node_add(
            node_add(node_mult(node_x(), node_x()),
                     node_mult(node_y(), node_y())),
                     node_mult(node_t(), node_t()))),
        .weight = 1,
    }));
    grammar_append_branches(grammar, &branches, SYMBOL("a"));

    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_rule(SYMBOL("a")),
        .weight = 2,
    }));
    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_add(node_rule(SYMBOL("c")), node_rule(SYMBOL("c"))),
        .weight = 3,
    }));
    context_da_append(&branches, ((Grammar_Branch) {
        .node = node_mult(node_rule(SYMBOL("c")), node_rule(SYMBOL("c"))),
        .weight = 3,
    }));
    grammar_append_branches(grammar, &branches, SYMBOL("c"));
    return SYMBOL("e");
}

bool compile_node_into_fragment_expression(String_Builder *sb, Node *expr, size_t level)
{
    switch (expr->kind) {
    case NK_X: sb_append_cstr(sb, "x"); break;
    case NK_Y: sb_append_cstr(sb, "y"); break;
    case NK_S: sb_append_cstr(sb, "s"); break;
    case NK_T: sb_append_cstr(sb, "t"); break;

    case NK_RULE:
    case NK_RANDOM:
        printf("%s:%d: ERROR: cannot compile a node that valid only for grammar definitions\n", expr->file, expr->line);
        return false;

    case NK_NUMBER: {
        size_t checkpoint = nob_temp_save();
        sb_append_cstr(sb, temp_sprintf("%f", expr->as.number));
        nob_temp_rewind(checkpoint);
    } break;

    case NK_BOOLEAN:
        sb_append_cstr(sb, expr->as.boolean ? "true" : "false");
        break;

    case NK_SQRT:
        sb_append_cstr(sb, "sqrt(");
        if (!compile_node_into_fragment_expression(sb, expr->as.unop, level + 1)) return false;
        sb_append_cstr(sb, ")");
        break;
    case NK_SIN:
        sb_append_cstr(sb, "sin(");
        if (!compile_node_into_fragment_expression(sb, expr->as.unop, level + 1)) return false;
        sb_append_cstr(sb, ")");
        break;

    case NK_ABS:
        sb_append_cstr(sb, "abs(");
        if (!compile_node_into_fragment_expression(sb, expr->as.unop, level + 1)) return false;
        sb_append_cstr(sb, ")");
        break;

    case NK_ADD:
        sb_append_cstr(sb, "(");
        if (!compile_node_into_fragment_expression(sb, expr->as.binop.lhs, level + 1)) return false;
        sb_append_cstr(sb, "+");
        if (!compile_node_into_fragment_expression(sb, expr->as.binop.rhs, level + 1)) return false;
        sb_append_cstr(sb, ")");
        break;

    case NK_MULT:
        sb_append_cstr(sb, "(");
        if (!compile_node_into_fragment_expression(sb, expr->as.binop.lhs, level + 1)) return false;
        sb_append_cstr(sb, "*");
        if (!compile_node_into_fragment_expression(sb, expr->as.binop.rhs, level + 1)) return false;
        sb_append_cstr(sb, ")");
        break;
    case NK_MOD:
        sb_append_cstr(sb, "mod(");
        if (!compile_node_into_fragment_expression(sb, expr->as.binop.lhs, level + 1)) return false;
        sb_append_cstr(sb, ",");
        if (!compile_node_into_fragment_expression(sb, expr->as.binop.rhs, level + 1)) return false;
        sb_append_cstr(sb, ")");
        break;
    case NK_GT:
        sb_append_cstr(sb, "(");
        if (!compile_node_into_fragment_expression(sb, expr->as.binop.lhs, level + 1)) return false;
        sb_append_cstr(sb, ">");
        if (!compile_node_into_fragment_expression(sb, expr->as.binop.rhs, level + 1)) return false;
        sb_append_cstr(sb, ")");
        break;

    case NK_TRIPLE:
        sb_append_cstr(sb, "vec3(");
        if (!compile_node_into_fragment_expression(sb, expr->as.triple.first, level + 1)) return false;
        sb_append_cstr(sb, ",");
        if (!compile_node_into_fragment_expression(sb, expr->as.triple.second, level + 1)) return false;
        sb_append_cstr(sb, ",");
        if (!compile_node_into_fragment_expression(sb, expr->as.triple.third, level + 1)) return false;
        sb_append_cstr(sb, ")");
        break;

    case NK_IF:
        sb_append_cstr(sb, "(");
        if (!compile_node_into_fragment_expression(sb, expr->as.iff.cond, level + 1)) return false;
        sb_append_cstr(sb, "?");
        if (!compile_node_into_fragment_expression(sb, expr->as.iff.then, level + 1)) return false;
        sb_append_cstr(sb, ":");
        if (!compile_node_into_fragment_expression(sb, expr->as.iff.elze, level + 1)) return false;
        sb_append_cstr(sb, ")");
        break;

    case COUNT_NK:
    default:
        UNREACHABLE("compile_node_into_fragment_expression");
    }
    return true;
}

// TODO: typechecking during compilation
bool compile_node_func_into_fragment_shader(String_Builder *sb, Node *f)
{
    sb_append_cstr(sb, "#version 330\n");
    sb_append_cstr(sb, "in vec2 fragTexCoord;\n");
    sb_append_cstr(sb, "out vec4 finalColor;\n");
    sb_append_cstr(sb, "uniform float time;\n");
    sb_append_cstr(sb, "vec4 map_color(vec3 rgb) {\n");
    sb_append_cstr(sb, "    return vec4((rgb + 1)/2.0, 1.0);\n");
    sb_append_cstr(sb, "}\n");
    sb_append_cstr(sb, "void main()\n");
    sb_append_cstr(sb, "{\n");
    sb_append_cstr(sb, "    float x = fragTexCoord.x*2.0 - 1.0;\n");
    sb_append_cstr(sb, "    float y = fragTexCoord.y*2.0 - 1.0;\n");
    sb_append_cstr(sb, "    float s = cos(time);\n");
    sb_append_cstr(sb, "    float t = sin(time);\n");
    sb_append_cstr(sb, "    finalColor = map_color(");
    if (!compile_node_into_fragment_expression(sb, f, 0)) return false;
    sb_append_cstr(sb, ");\n");
    sb_append_cstr(sb, "}\n");
    return true;
}

bool flag_int(int *argc, char ***argv, int *value)
{
    const char *flag = shift(*argv, *argc);
    if ((*argc) <= 0) {
        nob_log(ERROR, "No argument is provided for %s", flag);
        return false;
    }
    *value = atoi(shift(*argv, *argc));
    return true;
}

typedef enum {
    PUNCT_BAR,
    PUNCT_OPAREN,
    PUNCT_CPAREN,
    PUNCT_COMMA,
    PUNCT_SEMICOLON,
    COUNT_PUNCTS,
} Punct_Index;

const char *puncts[COUNT_PUNCTS] = {
    [PUNCT_BAR]       = "|",
    [PUNCT_OPAREN]    = "(",
    [PUNCT_CPAREN]    = ")",
    [PUNCT_COMMA]     = ",",
    [PUNCT_SEMICOLON] = ";",
};

const char *comments[] = {
    "#",
};

bool parse_node(Alexer *l, Node **node);

bool parse_pair(Alexer *l, Node **first, Node **second)
{
    Alexer_Token t = {0};
    alexer_get_token(l, &t);
    if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_OPAREN))) return false;

    if (!parse_node(l, first)) return false;

    alexer_get_token(l, &t);
    if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_COMMA))) return false;

    if (!parse_node(l, second)) return false;

    alexer_get_token(l, &t);
    if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_CPAREN))) return false;

    return true;
}

bool parse_triple(Alexer *l, Node **first, Node **second, Node **third)
{
    Alexer_Token t = {0};
    alexer_get_token(l, &t);
    if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_OPAREN))) return false;

    if (!parse_node(l, first)) return false;

    alexer_get_token(l, &t);
    if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_COMMA))) return false;

    if (!parse_node(l, second)) return false;

    alexer_get_token(l, &t);
    if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_COMMA))) return false;

    if (!parse_node(l, third)) return false;

    alexer_get_token(l, &t);
    if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_CPAREN))) return false;

    return true;
}

bool parse_node(Alexer *l, Node **node)
{
    Alexer_Token t = {0};
    alexer_get_token(l, &t);
    if (!alexer_expect_id(l, t, ALEXER_SYMBOL)) return false;
    if (alexer_token_text_equal_cstr(t, "random")) {
        *node = node_loc(t.loc.file_path, t.loc.row, NK_RANDOM);
    } else if (alexer_token_text_equal_cstr(t, "x")) {
        *node = node_loc(t.loc.file_path, t.loc.row, NK_X);
    } else if (alexer_token_text_equal_cstr(t, "y")) {
        *node = node_loc(t.loc.file_path, t.loc.row, NK_Y);
    } else if (alexer_token_text_equal_cstr(t, "s")) {
        *node = node_loc(t.loc.file_path, t.loc.row, NK_S);
    } else if (alexer_token_text_equal_cstr(t, "t")) {
        *node = node_loc(t.loc.file_path, t.loc.row, NK_T);
    } else if (alexer_token_text_equal_cstr(t, "sqrt")) {
        alexer_get_token(l, &t);
        if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_OPAREN))) return false;

        Node *unop;
        if (!parse_node(l, &unop)) return false;
        *node = node_unop_loc(t.loc.file_path, t.loc.row, NK_SQRT, unop);

        alexer_get_token(l, &t);
        if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_CPAREN))) return false;
    } else if (alexer_token_text_equal_cstr(t, "sin")) {
        alexer_get_token(l, &t);
        if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_OPAREN))) return false;

        Node *unop;
        if (!parse_node(l, &unop)) return false;
        *node = node_unop_loc(t.loc.file_path, t.loc.row, NK_SIN, unop);

        alexer_get_token(l, &t);
        if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_CPAREN))) return false;
    } else if (alexer_token_text_equal_cstr(t, "abs")) {
        alexer_get_token(l, &t);
        if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_OPAREN))) return false;

        Node *unop;
        if (!parse_node(l, &unop)) return false;
        *node = node_unop_loc(t.loc.file_path, t.loc.row, NK_ABS, unop);

        alexer_get_token(l, &t);
        if (!alexer_expect_id(l, t, ALEXER_ID(ALEXER_PUNCT, PUNCT_CPAREN))) return false;
    } else if (alexer_token_text_equal_cstr(t, "add")) {
        Node *lhs, *rhs;
        if (!parse_pair(l, &lhs, &rhs)) return false;
        *node = node_binop_loc(t.loc.file_path, t.loc.row, NK_ADD, lhs, rhs);
    } else if (alexer_token_text_equal_cstr(t, "mult")) {
        Node *lhs, *rhs;
        if (!parse_pair(l, &lhs, &rhs)) return false;
        *node = node_binop_loc(t.loc.file_path, t.loc.row, NK_MULT, lhs, rhs);
    } else if (alexer_token_text_equal_cstr(t, "vec3")) {
        Node *first, *second, *third;
        if (!parse_triple(l, &first, &second, &third)) return false;
        *node = node_triple_loc(t.loc.file_path, t.loc.row, first, second, third);
    } else {
        *node = node_rule(t);
    }
    return true;
}

bool parse_grammar_branch(Alexer *l, Grammar_Branch *branch)
{
    Alexer_Token t = {0};
    Alexer_State s = alexer_save(l);
    alexer_get_token(l, &t);
    while (t.id == ALEXER_ID(ALEXER_PUNCT, PUNCT_BAR)) {
        branch->weight += 1;
        s = alexer_save(l);
        alexer_get_token(l, &t);
    }
    alexer_rewind(l, s);

    if (!parse_node(l, &branch->node)) return false;
    return true;
}

bool parse_grammar_branches(Alexer *l, Alexer_Token name, Grammar_Branches *branches)
{
    Alexer_Token t = {0};
    branches->name = name;
    uint64_t branch_start[] = {
        ALEXER_ID(ALEXER_PUNCT, PUNCT_BAR), 
        ALEXER_ID(ALEXER_PUNCT, PUNCT_SEMICOLON),
    };
    bool quit = false;
    while (!quit) {
        Alexer_State s = alexer_save(l);
        alexer_get_token(l, &t);
        if (!alexer_expect_one_of_ids(l, t, branch_start, ARRAY_LEN(branch_start))) return false;
        switch (t.id) {
        case ALEXER_ID(ALEXER_PUNCT, PUNCT_BAR): {
            alexer_rewind(l, s);
            Grammar_Branch branch = {};
            if (!parse_grammar_branch(l, &branch)) return false;
            context_da_append(branches, branch);
        } break;
        case ALEXER_ID(ALEXER_PUNCT, PUNCT_SEMICOLON): quit = true; break;
        default: UNREACHABLE("parse_grammar_branches");
        }
    }
    branches->weight_sum = 0;
    for (size_t i = 0; i < branches->count; ++i) {
        branches->weight_sum += branches->items[i].weight;
    }
    return true;
}

bool parse_grammar(Alexer *l, Grammar *grammar)
{
    Alexer_Token t = {0};
    uint64_t top_level_start[] = { ALEXER_SYMBOL, ALEXER_END };
    bool quit = false;
    while (!quit) {
        alexer_get_token(l, &t);
        if (!alexer_expect_one_of_ids(l, t, top_level_start, ARRAY_LEN(top_level_start))) return false;
        switch (t.id) {
        case ALEXER_SYMBOL: {
            Grammar_Branches branches = {0};
            if (!parse_grammar_branches(l, t, &branches)) return false;
            context_da_append(grammar, branches);
        } break;
        case ALEXER_END: quit = true; break;
        default: UNREACHABLE("top_level");
        }
    }
    return true;
}

int main(int argc, char **argv)
{
    const char *program_name = shift(argv, argc);

    int depth = 30;
    int seed = time(0);
    int width = 16*100;
    int height = 9*100;
    int fps = 60;

    while (argc > 0) {
        const char *flag = argv[0];
        if (strcmp(flag, "-seed") == 0) {
            if (!flag_int(&argc, &argv, &seed)) return 1;
        } else if (strcmp(flag, "-depth") == 0) {
            if (!flag_int(&argc, &argv, &depth)) return 1;
        } else if (strcmp(flag, "-width") == 0) {
            if (!flag_int(&argc, &argv, &width)) return 1;
        } else if (strcmp(flag, "-height") == 0) {
            if (!flag_int(&argc, &argv, &height)) return 1;
        } else if (strcmp(flag, "-fps") == 0) {
            if (!flag_int(&argc, &argv, &fps)) return 1;
        } else {
            break;
        }
    }

    if (argc <= 0) {
        nob_log(ERROR, "Usage: [options] %s <command>", program_name);
        nob_log(ERROR, "No command is provided");
        return 1;
    }

    const char *command_name = shift(argv, argc);

    if (strcmp(command_name, "file") == 0) {
        if (argc <= 0) {
            nob_log(ERROR, "Usage: %s %s <output-path>", program_name, command_name);
            nob_log(ERROR, "No output path is provided");
            return 1;
        }
        const char *output_path = shift(argv, argc);

        if (argc > 0) {
            nob_log(ERROR, "Usage: %s %s <output-path>", program_name, command_name);
            nob_log(ERROR, "%s accepts only 1 argument", command_name);
            return 1;
        }

        Grammar grammar = {0};
        Alexer_Token entry = default_grammar(&grammar);

        srand(seed);
        nob_log(INFO, "SEED: %d", seed);
        nob_log(INFO, "DEPTH: %d", depth);
        nob_log(INFO, "WIDTH: %d", width);
        nob_log(INFO, "HEIGHT: %d", height);

        Node *f = gen_rule(grammar, entry, depth);
        if (!f) {
            nob_log(ERROR, "The crappy generation process could not terminate");
            return 1;
        }

        Image image = GenImageColor(width, height, BLANK);
        nob_log(INFO, "Generating image...");
        if (!render_image(image, f)) return 1;
        if (!ExportImage(image, output_path)) return 1;

        return 0;
    }

    if (strcmp(command_name, "gui") == 0) {
        if (argc <= 0) {
            nob_log(ERROR, "Usage: %s %s <input>", program_name, command_name);
            nob_log(ERROR, "no input is provided");
            return 1;
        }

        const char *input_path = shift(argv, argc);

        String_Builder src = {0};
        if (!read_entire_file(input_path, &src)) return 1;

        Alexer l = alexer_create(input_path, src.items, src.count);
        l.puncts = puncts;
        l.puncts_count = COUNT_PUNCTS;
        l.sl_comments = comments;
        l.sl_comments_count = ARRAY_LEN(comments);
        Grammar grammar = {0};
        if (!parse_grammar(&l, &grammar)) return 1;

        assert(grammar.count > 0);
        Alexer_Token entry = grammar.items[0].name;

        srand(seed);
        nob_log(INFO, "SEED: %d", seed);
        nob_log(INFO, "DEPTH: %d", depth);
        nob_log(INFO, "WIDTH: %d", width);
        nob_log(INFO, "HEIGHT: %d", height);
        nob_log(INFO, "FPS: %d", fps);

        Node *f = gen_rule(grammar, entry, depth);
        if (!f) {
            nob_log(ERROR, "The crappy generation process could not terminate");
            return 1;
        }

        String_Builder sb = {0};
        if (!compile_node_func_into_fragment_shader(&sb, f)) return 1;
        sb_append_null(&sb);

        FFMPEG *ffmpeg = NULL;

        InitWindow(width, height, "RandomArt");
        RenderTexture2D screen = LoadRenderTexture(width, height);
        Shader shader = LoadShaderFromMemory(NULL, sb.items);
        int time_loc = GetShaderLocation(shader, "time");
        SetTargetFPS(fps);
        SetExitKey(KEY_NULL);
        Texture default_texture = {
            .id = rlGetTextureIdDefault(),
            .width = 1,
            .height = 1,
            .mipmaps = 1,
            .format = PIXELFORMAT_UNCOMPRESSED_R8G8B8A8,
        };
        float time = 0.0f;
        float max_render_length = (2*PI)*2;
        bool pause = false;
        while (!WindowShouldClose()) {
            float w = GetScreenWidth();
            float h = GetScreenHeight();
            float dt = GetFrameTime();
            BeginDrawing();
            if (ffmpeg == NULL) {
                SetShaderValue(shader, time_loc, &time, SHADER_UNIFORM_FLOAT);
                BeginShaderMode(shader);
                    DrawTexturePro(
                            default_texture,
                            (Rectangle){0, 0, 1, 1},
                            (Rectangle){0, 0, w, h},
                            (Vector2){0}, 0, WHITE);
                EndShaderMode();
                if (!pause) time += dt;

                if (IsKeyPressed(KEY_R)) {
                    ffmpeg = ffmpeg_start_rendering(width, height, fps);
                    time = 0;
                    SetTraceLogLevel(LOG_WARNING);
                }
                if (IsKeyPressed(KEY_SPACE)) {
                    pause = !pause;
                }
            } else {
                if (time < max_render_length) {
                    BeginTextureMode(screen);
                        SetShaderValue(shader, time_loc, &time, SHADER_UNIFORM_FLOAT);
                        BeginShaderMode(shader);
                            DrawTexturePro(
                                    default_texture,
                                    (Rectangle){0, 0, 1, 1},
                                    (Rectangle){0, 0, w, h},
                                    (Vector2){0}, 0, WHITE);
                        EndShaderMode();
                    EndTextureMode();

                    DrawTexture(screen.texture, 0, 0, WHITE);

                    // Progress bar
                    {
                        Rectangle bar_frame = {0};
                        bar_frame.width = w*.85f;
                        bar_frame.height = h*.10f;
                        bar_frame.x = w*.5f - bar_frame.width*.5f;
                        bar_frame.y = h - bar_frame.height*2.f;

                        float shadow_offset = 0.004f;

                        Rectangle shadow_bar_frame = bar_frame;
                        shadow_bar_frame.x -= w*shadow_offset;
                        shadow_bar_frame.y += w*shadow_offset;

                        Rectangle progress_bar = bar_frame;
                        progress_bar.width *= time/max_render_length;

                        float thick = w*.01f;
                        DrawRectangleLinesEx(shadow_bar_frame, thick, BLACK);
                        DrawRectangleRec(progress_bar, WHITE);
                        DrawRectangleLinesEx(bar_frame, thick, WHITE);
                    }

                    Image image = LoadImageFromTexture(screen.texture);
                    ffmpeg_send_frame_flipped(ffmpeg, image.data, width, height);
                    UnloadImage(image);

                    time += 1.0f/fps;

                    if (IsKeyPressed(KEY_ESCAPE)) {
                        time = 0;
                        ffmpeg_end_rendering(ffmpeg);
                        ffmpeg = NULL;
                        SetTraceLogLevel(LOG_INFO);
                    }
                } else {
                    time = 0;
                    ffmpeg_end_rendering(ffmpeg);
                    ffmpeg = NULL;
                    SetTraceLogLevel(LOG_INFO);
                }
            }
            EndDrawing();
        }
        CloseWindow();
        return 0;
    }

    if (strcmp(command_name, "parse") == 0) {
    }

    nob_log(ERROR, "Unknown command %s", command_name);
    return 1;
}
