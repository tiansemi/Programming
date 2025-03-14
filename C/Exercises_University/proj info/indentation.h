
#include <stdbool.h>
#ifndef INDENTATIONH
#define INDENTATIONH

typedef struct Node {
    char data;
    struct Node* next;
} Node;

typedef struct Stack{
    Node* top;
} Stack;

void initialize_stack(Stack* stack);
void push(Stack* stack, char data);
char pop(Stack* stack);
int is_empty(Stack* stack);
bool is_for_loop(FILE* input);
bool is_include(FILE* input);

void indent_file(FILE* input, FILE* output);

#endif