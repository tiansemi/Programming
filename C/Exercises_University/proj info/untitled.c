#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "indentation.h"

#define INDENT_SIZE 4

void initialize_stack(Stack* stack) {
    stack->top = NULL;
}

void push(Stack* stack, char data) {
    Node* new_node = (Node*)malloc(sizeof(Node));
    if (new_node == NULL) {
        perror("Memory allocation error");
        exit(EXIT_FAILURE);
    }

    new_node->data = data;
    new_node->next = stack->top;
    stack->top = new_node;
}

char pop(Stack* stack) {
    if (is_empty(stack)) {
        fprintf(stderr, "Error: Trying to pop from an empty stack\n");
        exit(EXIT_FAILURE);
    }

    char data = stack->top->data;
    Node* temp = stack->top;
    stack->top = temp->next;
    free(temp);

    return data;
}

int is_empty(Stack* stack) {
    return stack->top == NULL;
}

bool is_for_loop(FILE* input) {
    char buffer[23]; 
    int i;

    buffer[0] = 'f';
    for (i = 1; i <= 22; i++) {
        char ch = fgetc(input);
        if (ch == EOF || ch == '\n') {
            break;
        }
        buffer[i] = ch;
    }
    buffer[i] = '\0';

    if (strncmp(buffer, "for(", 4) == 0) {
        // Rembobiner le nombre de caractères lus pour ne pas perdre de données
        fseek(input,-(i-1),SEEK_CUR);

        // La chaîne commence par "for(" ; c'est une boucle for
        return true;
    }
    fseek(input,-(i-1),SEEK_CUR);
    return false;
}

bool is_include(FILE* input) {
    char buffer[25]; 
    int i;

    buffer[0] = '#';
    for (i = 1; i <= 22; i++) {
        char ch = fgetc(input);
        if (ch == EOF || ch == '\n') {
            break;
        }
        buffer[i] = ch;
    }
    buffer[i] = '\0';

    if (strncmp(buffer, "#include", 8) == 0) {

        fseek(input,-i,SEEK_CUR);
        return true;
    }
    fseek(input,-i,SEEK_CUR);
    return false;
}

void indent_file(FILE* input, FILE* output) {
    Stack stack;
    initialize_stack(&stack);

    int ch ;
    int indentation = 0 , is_for=0 , is_inc = 0;

    while ((ch = fgetc(input)) != EOF) { 

        if (ch == '}') {
            indentation -= INDENT_SIZE;
            fputc('\n',output);

            while (!is_empty(&stack) && stack.top->data != '{') {
                pop(&stack);
            }
            
            if (!is_empty(&stack)) {
                pop(&stack);  // Pop the '{'
            }
        }

        fputc(ch, output);

        if (ch == '{') {
            indentation += INDENT_SIZE;
            push(&stack, '{');
            fputc('\n',output);
            for (int i = 0; i < indentation; ++i) {
                fputc(' ', output);
            }
        }

        is_for = (ch == 'f' && is_for_loop(input) ) ? 1 : is_for ;
        // if (ch=='f'){
        //     if (is_for_loop(input)){
        //         is_for=1;
        //     }else
        //         is_for=0;
        // }
        if (ch == ';' && !is_for) {
            fputc('\n', output);
            for (int i = 0; i < indentation; i++) {
                fputc(' ', output);
            }
        }
        is_for = (ch == ')' && is_for) ? 0 : is_for ;

        is_inc = (ch == '#' && is_include(input)) ? 1 : is_inc ;
        // if (ch=='#'){
        //     if (is_include(input)){
        //         is_inc=1;
        //     }else
        //         is_inc=0;
        // }
        if (ch == '>' && is_inc) {
            fputc('\n', output);
        }
        if(ch=='"' && is_inc){
            while ( (ch=fgetc(input)) != '"' && ch != EOF)
                fputc(ch,output);
            fputc(ch,output);
            fputc('\n',output);
        }
    }
}