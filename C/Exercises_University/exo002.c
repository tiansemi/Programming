/*
Enoncé : 
1) Soit le vecteur d’entier V de N entiers (N est connu). Le vecteur est déclaré de la manière suivante : int* V. 
Ecrire un code en C qui permet de réserver de la mémoire pour V en utilisant la fonction malloc.
2) Nous souhaitons manipuler une matrice d’entiers à k lignes et n colonnes. Cette matrice est déclarée de la manière suivante : int **matrice ;
On suppose que les variables k et n sont connues. Ecrire le code C qui permet la réservation de la matrice en mémoire à l'aide de la fonction malloc.
3) Soit int T[N], un tableau à une dimension. T[i] est équivalent à *(T+i).
Soient la déclaration du tableau suivant : int Tab [10][20][5]; On souhaite accéder à l'élément Tab[2][5][3]. 
En s'inspirant de la première ligne, donner une écriture équivalente de Tab[2][5][3] qui utilise exclusivement les pointeurs sans la présence de crochet.

Solution : 
1) L'instruction à utilisé est `V = (int*)malloc(N*sizeof(int));`
//-- Pour tester
#include <stdio.h>
#include <stdlib.h>

int main() {
    int N = 10 , *tab[10];  // Remplacez N par la taille désirée de votre vecteur.
    int* V;

    // Allocation de mémoire pour le vecteur V.
    V = (int*)malloc(N * sizeof(int));

    if (V == NULL) {
        printf("Échec de l'allocation de mémoire pour V.\n");
        exit(1);
    }

    // Vous pouvez maintenant utiliser le vecteur V.

    // N'oubliez pas de libérer la mémoire lorsque vous avez terminé.
    for (int i = 0; i < N; i++) {
        V[i] = i * 100;
    }

    // Afficher les adresses des cases du tableau.
    for (int i = 0; i < N; i++) {
        printf("Adresse de V[%d] : %p\n", i, (void*)&V[i]);
    }
    free(V);

    return 0;
}
//-- Fin du code à testé

2) Les instructions à utilisés sont `matrice = (int**)malloc(k*sizeof(int));
    for(int i=0;i<k;i++){ matrice[i]=(int*)malloc(n*sizeof(int)); }` 
//-- Pour tester
#include <stdio.h>
#include <stdlib.h>

int main() {
    int k = 5;  // Remplacez k par le nombre de lignes souhaité.
    int n = 10; // Remplacez n par le nombre de colonnes souhaité.
    int** matrice;
    int i; // Pour parcourir la boucle

    // Allocation de mémoire pour les pointeurs de lignes.
    matrice = (int**)malloc(k * sizeof(int*));

    if (matrice == NULL) {
        printf("Échec de l'allocation de mémoire pour les pointeurs de lignes.\n");
        exit(1);
    }

    // Allocation de mémoire pour chaque ligne.
    for (i = 0; i < k; i++) {
        matrice[i] = (int*)malloc(n * sizeof(int));
        if (matrice[i] == NULL) {
            printf("Échec de l'allocation de mémoire pour une ligne.\n");
            exit(1);
        }
    }

    // Afficher les adresses des cases du tableau.
    for (int i = 0; i < k; i++) {
        for (int j = 0; j < n; j++) {
            printf("Adresse de matrice[%d][%d] : %p\n", i+1,j+1, (void*)&matrice[i][j]);
        }
    }


    // N'oubliez pas de libérer la mémoire lorsque vous avez terminé.
    for (i = 0; i < k; i++) {
        free(matrice[i]);
    }
    free(matrice);

    return 0;
}
//-- Fin du code à testé

3) L'écriture équivalente est `*(*(*(Tab+2)+5)+3)`
//-- Pour tester
#include <stdio.h>
#include <stdlib.h>

int main() {
    int Tab[10][20][5];
    printf("\n\n%p\n%p",&Tab[2][5][3],(*(*(Tab+2)+5)+3) );

    return 0;
}
//-- Fin du code à testé

*/
