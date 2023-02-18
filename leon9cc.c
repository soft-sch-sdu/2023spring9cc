#include <assert.h>
#include <stdarg.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Tokenizer

enum {
    TK_NUM = 256, // Number literal
    TK_EOF,       // End marker
};

// Token type
typedef struct {
    int ty;      // Token type
    int val;     // Number literal
    char *input; // Token string (for error reporting)
} Token;

// Tokenized input is stored to this array.
Token tokens[100];

void tokenize(char *p) {
    int i = 0;
    while (*p) {
        // Skip whitespace
        if (isspace(*p)) {
            p++;
            continue;
        }

        // + or -
        if (*p == '+' || *p == '-') {
            tokens[i].ty = *p;
            tokens[i].input = p;
            i++;
            p++;
            continue;
        }

        // Number
        if (isdigit(*p)) {
            tokens[i].ty = TK_NUM;
            tokens[i].input = p;
            tokens[i].val = strtol(p, &p, 10);
            i++;
            continue;
        }

        fprintf(stderr, "cannot tokenize: %s", p);
        exit(1);
    }

    tokens[i].ty = TK_EOF;
}

// Recursive-descendent parser

int pos = 0;

enum {
    ND_NUM = 256,     // Number literal
};

typedef struct Node {
    int ty;           // Node type
    struct Node *lhs; // left-hand side
    struct Node *rhs; // right-hand side
    int val;          // Number literal
} Node;

Node *new_node(int op, Node *lhs, Node *rhs) {
    Node *node = malloc(sizeof(Node));
    node->ty = op;
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *new_node_num(int val) {
    Node *node = malloc(sizeof(Node));
    node->ty = ND_NUM;
    node->val = val;
    return node;
}

void error(char *fmt, ...) { // An error reporting function.
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

Node *number() {
    if (tokens[pos].ty == TK_NUM)
        return new_node_num(tokens[pos++].val);
    error("number expected, but got %s", tokens[pos].input);
}

Node *expr() {
    Node *lhs = number();
    for (;;) {
        int op = tokens[pos].ty;
        if (op != '+' && op != '-')
            break;
        pos++;
        lhs = new_node(op, lhs, number());
    }

    if (tokens[pos].ty != TK_EOF)
        error("stray token: %s", tokens[pos].input);
    return lhs;
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

IR *new_ir(int op, int lhs, int rhs) {
    IR *ir = malloc(sizeof(IR));
    ir->op = op;
    ir->lhs = lhs;
    ir->rhs = rhs;
    return ir;
}

IR *instructions[1000];
int inp;
int regno;

// ir_sub的意思是IR subclass
int gen_ir_sub(Node *node) {
    if (node->ty == ND_NUM) {
        int registerNo = regno++;
        // instructions[指令(执行时的)序列号] = new_ir(指令类型，地址(寄存器编号)，值)
        instructions[inp++] = new_ir(IR_IMM, registerNo, node->val);
        return registerNo;
    }

    assert(node->ty == '+' || node->ty == '-');
    // 不管当前AST结点是+还是-，先左分支后右分支，分别生成TAC指令，并分别将result地址存于lhs和rhs.
    // 从这里也可以看出，lhs与rhs都是代表地址，即寄存器编号.
    // 注意，这里每条TAC指令都分配一个新的寄存器，用来保存该指令的结果.
    int lhs = gen_ir_sub(node->lhs);
    int rhs = gen_ir_sub(node->rhs);

    // 根据左右结果，生成当前+或-对应的TAC指令，将指令添加到指令序列末尾，那么将来结果放到哪个寄存器？继续往下看...
    instructions[inp++] = new_ir(node->ty, lhs, rhs);
    // 生成特殊指令KILL，将右结果(操作数)占用的寄存器释放
    instructions[inp++] = new_ir(IR_KILL, rhs, 0); // 准备好，将来执行TAC指令序列时，把右操作数占用的寄存器还回去
    // 而将来的结果则放在左操作数之前占用的寄存器
    return lhs;
}

void gen_ir(Node *node) {
    int registerNo = gen_ir_sub(node);
    instructions[inp++] = new_ir(IR_RETURN, registerNo, 0);
}

// Register allocator

char *regs[] = {"%rdi", "%rsi", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"};
bool used[8];

int reg_map[1000];

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

void alloc_regs() {
    for (int i = 0; i < inp; i++) {
        IR *ir = instructions[i];

        switch (ir->op) {
            case IR_IMM:
                ir->lhs = alloc(ir->lhs);
                break;
            case IR_MOV:
            case '+':
            case '-':
                ir->lhs = alloc(ir->lhs);
                ir->rhs = alloc(ir->rhs);
                break;
            case IR_RETURN:
                kill(reg_map[ir->lhs]);
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

void gen_x86() {
    for (int i = 0; i < inp; i++) {
        IR *ir = instructions[i];

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
            case IR_NOP:
                break;
            default:
                assert(0 && "unknown operator");
        }
    }
}




int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: leon9cc <code>\n");
        return 1;
    }


    for (int i = 0; i < sizeof(reg_map) / sizeof(*reg_map); i++)
        reg_map[i] = -1;

    // Tokenize and parse.
    tokenize(argv[1]);
    Node* node = expr();

    gen_ir(node);
    alloc_regs();

    printf("  .global main\n");
    printf("main:\n");

    gen_x86();

    return 0;
}