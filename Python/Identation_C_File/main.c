#include <stdio.h>
#include <stdlib.h>

// Structure pour un element de la pile
typedef struct Noeud {
    char info;
    struct Noeud* suivant;
} Noeud;

// Structure pour la pile
typedef struct Pile {
    Noeud* sommet;
} Pile;

// Initialiser la pile
void initialisation(Pile* pile) {
    pile->sommet = NULL;
}

// Verifier si la pile est vide
int est_vide(Pile* pile) {
    return pile->sommet == NULL;
}

// Empiler un element sur la pile
void empiler(Pile* pile, char info) {
    Noeud* pt = (Noeud*)malloc(sizeof(Noeud));
    pt->info = info;
    pt->suivant = pile->sommet;
    pile->sommet = pt;
}

// Depiler un element de la pile
char depiler(Pile* pile) {
    if (est_vide(pile)) {
        return '\0';
    }
    Noeud* temp = pile->sommet;
    char info = temp->info;
    pile->sommet = pile->sommet->suivant;
    free(temp);
    return info;
}

// Obtenir l'element en haut de la pile sans le depiler
char obtenir_sommet(Pile* pile) {
    if (est_vide(pile)) {
        return '\0';
    }
    return pile->sommet->info;
}

// Fonction pour indenter le code C
void indentation_programme(FILE* inputFile, FILE* outputFile) {
    if (inputFile == NULL) {
        printf("Unable to open input file.\n");
        return;
    }
    if (outputFile == NULL) {
        printf("Unable to create output file.\n");
        fclose(inputFile);
        return;
    }

    Pile pile;
    initialisation(&pile);
    char c;
    int niveau_indentation = 0;

    while ((c = fgetc(inputFile)) != EOF) {
        if (c == '{') {
            niveau_indentation++;
            if (obtenir_sommet(&pile) == '}') {
                niveau_indentation--;
            }
            fputc(c, outputFile);
            fputc('\n', outputFile);
            for (int i = 0; i < niveau_indentation; i++) {
                fputc('\t', outputFile);
            }
            empiler(&pile, c);
        } else if (c == '}') {
            printf("%d\n",niveau_indentation);
            niveau_indentation--;
            printf("%d\n",niveau_indentation);
            if (niveau_indentation < 0) {
                printf("Syntax error: too many block closings '}'.\n");
                fclose(inputFile);
                fclose(outputFile);
                return;
            }
            depiler(&pile);
            fseek(outputFile,-1,SEEK_CUR);
            fputc(c, outputFile);
            fputc('\n', outputFile);
            for (int i = 0; i < niveau_indentation; i++) {
                fputc('\t', outputFile); 
            }
        }
        else if (c == ';') {
            fputc(c, outputFile);
            fputc('\n', outputFile);
            for (int i = 0; i < niveau_indentation; i++) {
                fputc('\t', outputFile); 
            }
        }
         else {
            fputc(c, outputFile);
        }
    }

    if (!est_vide(&pile)) {
        printf("Syntax error: too many block openings '{'.\n");
    }
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("Please provide file name in command line.\n");
        return 1;
    }
    FILE* inputFile = fopen(argv[1], "r");
    if (inputFile == NULL) {
        printf("Unable to open file for reading : %s\n", argv[1]);
        return 1;
    }
    FILE* outputFile;
    if (argc == 2) {
        outputFile = fopen("output.c", "w");
    } else {
        outputFile = fopen(argv[2], "w");
    }
    if (outputFile == NULL) {
        printf("Unable to create output file : %s\n", argc == 2 ? "output.c" : argv[2]);
        fclose(inputFile);
        return 1;
    }
    indentation_programme(inputFile, outputFile);
    fclose(inputFile);
    fclose(outputFile);
    printf("Indentation completed successfully.\n");
    return 0;
}
