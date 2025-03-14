
#include <stdio.h>
#include <stdlib.h>
#include "indentation.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s input_file [output_file]\n", argv[0]);
        return 1;
    }

    FILE *input_file = fopen(argv[1], "r");
    if (input_file == NULL) {
        perror("Error opening input file");
        return 1;
    }

    FILE *output_fe = argc == 3 ? fopen(argv[2], "w") : stdout;
    if (output_fe == NULL) {
        perror("Error opening output file");
        fclose(input_file);
        return 1;
    }
    
    indent_file(input_file, output_fe);

    fclose(input_file);
    if (output_fe != stdout) {
        fclose(output_fe);
    }

    return 0;
}