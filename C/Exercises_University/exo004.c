/*
Enoncé :
Soit Etudiant.txt un fichier texte où chaque ligne représente les données d’un étudiant, avec 
le matricule, le nom, les prénoms et l'année de naissance de l'étudiant. Un exemple d'une ligne de ce fichier est :
5024	Zando	Marcel PagnoL	1998
8796	Mani	Zeguen Mathieu 1997
L'objectif de cet exercice est de créer et remplir un fichier binaire d’étudiants nommé Etudiant.dat 
à partir des données issues du fichier texte. L’exécution du programme à écrire acceptera deux arguments en ligne de commande.
Les deux arguments correspondent respectivement au nom du fichier texte et celui du fichier binaire.
1)	Donner en C les structures de données nécessaires pour cette application
2)	Ecrire le programme C correspondant

Solution :
1)
`typedef struct Etudiant{
    int matricule;
    char nom[50];
    char prenoms[150];
    int annee_naissance;
}Etudiant;`

2)
`
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int matricule;
    char nom[6];
    char prenoms[15];
    int annee_naissance;
}Etudiant;

int main(int argc, const char* argv[]) {
    if (argc != 3) {
        printf("Utilisation : %s <fichier_texte> <fichier_binaire>\n", argv[0]);
        return 1;
    }

    FILE *fichier_texte = fopen(argv[1],"r");
    FILE *fichier_binaire = fopen(argv[2],"wb+");

    if (fichier_binaire != NULL && fichier_texte!=NULL){
        Etudiant e;
        while(!feof(fichier_texte)){
            if(fscanf(fichier_texte, "%d %5c %14c %d", &e.matricule, e.nom, e.prenoms, &e.annee_naissance)) {
                fwrite(&e, sizeof(Etudiant), 1, fichier_binaire);
            } else {
                printf("Erreur de lecture du fichier_texte\n");
            }
        }
    }else{  printf("Erreur d'ouverture des fichiers\n"); }

    free(fichier_texte);
    free(fichier_binaire);

    return 0;
}
`
*/
