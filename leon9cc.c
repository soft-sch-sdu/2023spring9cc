#include <assert.h>
#include <stdarg.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>


// Vector （该数据结构，tokenizer会用到、生成IR会用到）
typedef struct {
    void **data; // 也可以这么理解：void *([] data)，或void *(* data))
    int capacity;
    int len;
} Vector;

Vector *new_vec() {
    Vector *v = malloc(sizeof(Vector));
    v->data = malloc(sizeof(void *) * 16);
    v->capacity = 16;
    v->len = 0;
    return v; //v->{ data->[*. *. *. ... *], 16(初始空间可容16个*.), 0(开始时啥也没有) }
}

void vec_push(Vector *v, void *elem) {
    if (v->len == v->capacity) {
        v->capacity *= 2;
        v->data = realloc(v->data, sizeof(void *) * v->capacity);
    }
    v->data[v->len++] = elem;
}

// map （tokenizer会用到该数据结构，处理keywords）
typedef struct {
    Vector *keys;
    Vector *vals;
} Map;

Map *new_map(void) {
    Map *map = malloc(sizeof(Map));
    map->keys = new_vec();
    map->vals = new_vec();
    return map;
}

void map_put(Map *map, char *key, void *val) {
    vec_push(map->keys, key);
    vec_push(map->vals, val);
}

void *map_get(Map *map, char *key) {
    for (int i = map->keys->len - 1; i >= 0; i--)
        if (!strcmp(map->keys->data[i], key))
            return map->vals->data[i];
    return NULL;
}


// An error reporting function.
void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}


// Tokenizer

Map *keywords;

enum {
    TK_NUM = 256, // Number literal
    TK_RETURN,    // "return"
    TK_EOF,       // End marker
};

typedef struct {
    int ty;      // Token type
    int val;     // Number literal
    char *input; // Token string (for error reporting)
} Token;

static Token *add_token(Vector *v, int ty, char *input) {
    Token *t = malloc(sizeof(Token));
    t->ty = ty;
    t->input = input;
    vec_push(v, t);
    return t;
}

// Tokenized input is stored to this array.
static Vector *scan(char *p) {
    Vector *v = new_vec();

    int i = 0;
    while (*p) {
        // Skip whitespace
        if (isspace(*p)) {
            p++;
            continue;
        }

        // Single-letter token
        if (strchr("+-*/;", *p)) {
            add_token(v, *p, p);
            i++;
            p++;
            continue;
        }

        // Keyword
        if (isalpha(*p) || *p == '_') {
            int len = 1;
            while (isalpha(p[len]) || isdigit(p[len]) || p[len] == '_')
                len++;

            char *name = strndup(p, len);
            int ty = (intptr_t)map_get(keywords, name);
            if (!ty)
                error("unknown identifier: %s", name);

            add_token(v, ty, p);
            i++;
            p += len;
            continue;
        }

        // Number
        if (isdigit(*p)) {
            Token *t = add_token(v, TK_NUM, p);
            t->val = strtol(p, &p, 10);
            i++;
            continue;
        }

        error("cannot tokenize: %s", p);
    }

    add_token(v, TK_EOF, p);
    return v;
}

Vector *tokenize(char *p) {
    keywords = new_map();
    map_put(keywords, "return", (void *)TK_RETURN);

    return scan(p);
}

// Recursive-descendent parser
enum {
    ND_NUM = 256,     // Number literal
    ND_RETURN,        // Return statement
    ND_COMP_STMT,     // Compound statement
    ND_EXPR_STMT,     // Expressions tatement
};

typedef struct Node {
    int ty;            // Node type
    struct Node *lhs;  // left-hand side
    struct Node *rhs;  // right-hand side
    int val;           // Number literal
    struct Node *expr; // "return" or expresson stmt
    Vector *stmts;     // Compound statement
} Node;

static Vector *tokens;
static int pos;

static void expect(int ty) {
    Token *t = tokens->data[pos];
    if (t->ty != ty)
        error("%c (%d) expected, but got %c (%d)", ty, ty, t->ty, t->ty);
    pos++;
}

static Node *new_node(int op, Node *lhs, Node *rhs) {
    Node *node = malloc(sizeof(Node));
    node->ty = op;
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

static Node *number() {
    Token *t = tokens->data[pos];
    if (t->ty != TK_NUM)
        error("number expected, but got %s", t->input);
    pos++;

    Node *node = malloc(sizeof(Node));
    node->ty = ND_NUM;
    node->val = t->val;
    return node;
}

static Node *mul() {
    Node *lhs = number();
    for (;;) {
        Token *t = tokens->data[pos];
        int op = t->ty;
        if (op != '*' && op != '/')
            return lhs;
        pos++;
        lhs = new_node(op, lhs, number());
    }
}

static Node *expr() {
    Node *lhs = mul();
    for (;;) {
        Token *t = tokens->data[pos];
        int op = t->ty;
        if (op != '+' && op != '-')
            return lhs;
        pos++;
        lhs = new_node(op, lhs, mul());
    }
}

static Node *stmt() {
    Node *node = malloc(sizeof(Node));
    node->ty = ND_COMP_STMT;
    node->stmts = new_vec();

    for (;;) {
        Token *t = tokens->data[pos];
        if (t->ty == TK_EOF)
            return node;

        Node *e = malloc(sizeof(Node));

        if (t->ty == TK_RETURN) {
            pos++;
            e->ty = ND_RETURN;
            e->expr = expr();
        } else {
            e->ty = ND_EXPR_STMT;
            e->expr = expr();
        }

        vec_push(node->stmts, e);
        expect(';');
    }
}
/******************CFG (or BNF)**************************
 *
 * program :=  stmt
 * stmt :=  (expr; | "return" expr;)+
 * expr :=  mul_div ("+" mul_div | "-" mul_div)*
 * mul_div :=  num_literal ("*" num_literal | "/" num_literal)*
 *
 *********************************************************/
Node *parse(Vector *v) {
    tokens = v;
    pos = 0;

    return stmt();
}

// Intermediate representation
enum { // TAC的指令类型，这里并没全部枚举出来，比如“+”和“-”的类型值是其ASCII码值
    IR_IMM,
    IR_MOV,
    IR_RETURN,
    IR_KILL,
    IR_NOP,
};

typedef struct {
    int op;
    int lhs;
    int rhs;
} IR;

static Vector *code;

IR *add(int op, int lhs, int rhs) {
    IR *ir = malloc(sizeof(IR));
    ir->op = op;
    ir->lhs = lhs;
    ir->rhs = rhs;
    vec_push(code, ir);
    return ir;
}

int gen_expr(Node *node) {
    static int regno;

    if (node->ty == ND_NUM) {
        int registerNo = regno++;
        add(IR_IMM, registerNo, node->val);
        return registerNo;
    }

    assert(strchr("+-*/", node->ty));
    // 不管当前AST结点是+还是-，先左分支后右分支，分别生成TAC指令，并分别将result地址存于lhs和rhs.
    // 从这里也可以看出，lhs与rhs都是代表地址，即寄存器编号.
    // 注意，这里每条TAC指令都分配一个新的寄存器(无限多个，足够用)，用来保存该指令的结果.
    int lhs = gen_expr(node->lhs);
    int rhs = gen_expr(node->rhs);

    // 根据左右结果，生成当前+或-对应的TAC指令，将指令添加到指令序列末尾，那么将来结果放到哪个寄存器？继续往下看...
    add(node->ty, lhs, rhs);
    // 生成特殊指令KILL，将右结果(操作数)占用的寄存器释放
    add(IR_KILL, rhs, 0); // 准备好，将来执行TAC指令序列时，把右操作数占用的寄存器还回去
    // 而将来的结果则放在左操作数之前占用的寄存器
    return lhs;
}

static void gen_stmt(Node *node) {
    if (node->ty == ND_RETURN) {
        int r = gen_expr(node->expr);
        add(IR_RETURN, r, 0);
        add(IR_KILL, r, 0);
        return;
    }

    if (node->ty == ND_EXPR_STMT) {
        int r = gen_expr(node->expr);
        add(IR_KILL, r, 0);
        return;
    }

    if (node->ty == ND_COMP_STMT) {
        for (int i = 0; i < node->stmts->len; i++)
            gen_stmt(node->stmts->data[i]);
        return;
    }

    error("unknown node: %d", node->ty);
}

Vector *gen_ir(Node *node) {
    assert(node->ty == ND_COMP_STMT);
    code = new_vec();
    gen_stmt(node);
    return code;
}

// Register allocator
char *regs[] = {"%rdi", "%rsi", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"};
bool used[8];

int *reg_map;

int alloc(int ir_reg) {
    if (reg_map[ir_reg] != -1) {
        int registerNo = reg_map[ir_reg];
        assert(used[registerNo]);
        return registerNo;
    }

    for (int i = 0; i < sizeof(regs) / sizeof(*regs); i++) {
        if (used[i])
            continue;
        used[i] = true;
        reg_map[ir_reg] = i;
        return i;
    }

    error("register exhausted");
}

void kill(int registerNo) {
    assert(used[registerNo]);
    used[registerNo] = false;
}

void alloc_regs(Vector *irv) {
    reg_map = malloc(sizeof(int) * irv->len);
    for (int i = 0; i < irv->len; i++)
        reg_map[i] = -1;

    for (int i = 0; i < irv->len; i++) {
        IR *ir = irv->data[i];

        switch (ir->op) {
            case IR_IMM:
                ir->lhs = alloc(ir->lhs);
                break;
            case IR_MOV:
            case '+':
            case '-':
            case '*':
            case '/':
                ir->lhs = alloc(ir->lhs);
                ir->rhs = alloc(ir->rhs);
                break;
            case IR_RETURN:
                ir->lhs = alloc(ir->lhs);
                break;
            case IR_KILL:
                kill(reg_map[ir->lhs]);
                ir->op = IR_NOP;
                break;
            default:
                assert(0 && "unknown operator");
        }
    }
}


// Code generator
void gen_x86(Vector *irv) {
    for (int i = 0; i < irv->len; i++) {
        IR *ir = irv->data[i];

        switch (ir->op) {
            case IR_IMM:
                printf("  mov $%d, %s\n", ir->rhs, regs[ir->lhs]);
                break;
            case IR_MOV:
                printf("  mov %s, %s\n", regs[ir->rhs], regs[ir->lhs]);
                break;
            case IR_RETURN:
                printf("  mov %s, %%rax\n", regs[ir->lhs]);
                printf("  ret\n");
                break;
            case '+':
                printf("  add %s, %s\n", regs[ir->rhs], regs[ir->lhs]);
                break;
            case '-':
                printf("  sub %s, %s\n", regs[ir->rhs], regs[ir->lhs]);
                break;
            case '*':
                printf("  imul %s, %s\n", regs[ir->rhs], regs[ir->lhs]);
                break;
            case '/':
                printf("  mov %s, %%rax\n", regs[ir->lhs]);
                printf("  cqo\n");
                printf("  div %s\n", regs[ir->rhs]);
                printf("  mov %%rax, %s\n", regs[ir->lhs]);
                break;
            case IR_NOP:
                break;
            default:
                assert(0 && "unknown operator");
        }
    }
}


// driver
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: leon9cc <code>\n");
        return 1;
    }

    // Tokenize and parse.
    tokens = tokenize(argv[1]);
    Node* node = parse(tokens);

    Vector *irv = gen_ir(node);
    alloc_regs(irv);

    printf("  .global main\n");
    printf("main:\n");

    gen_x86(irv);

    return 0;
}