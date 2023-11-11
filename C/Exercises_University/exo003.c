/*
Enoncé :
Soit la déclaration suivante :
typedef struct Cellule {
	int info;
	struct Cellule* suiv;
} MaiLLon ;

On rappelle qu'une liste chaînée d'entiers est une structure de donnée dans laquelle chaque élément de type Maillon 
contient l'adresse mémoire d’un élément du même type (élément suivant). Le dernier élément pointe sur NULL.
a) On suppose que la liste existe déjà et est représentée par l’adresse de son premier élément (tête de liste).
Ecrire une fonction C, nommée insérer qui permet d'insérer un élément en tête de liste de telle sorte que 
cet élément devienne la tête de liste. Cette fonction est paramétrée par le nombre à insérer ainsi que la liste.
b) On souhaite sauvegarder la liste dans un fichier pour une restauration ultérieure. 
Ecrire une fonction de sauvegarde de la liste paramétrée par le nom du fichier et la liste.

Solution :
a) 
`
Maillon* inserer(int nombre, Maillon* liste) {
    Maillon* nouveau_maillon = (Maillon*)malloc(sizeof(Maillon));

    if (nouveau_maillon != NULL) {
    	nouveau_maillon->info = nombre;
        nouveau_maillon->suiv = liste;
        
        return nouveau_maillon;
    }
}
`
// -- Pour tester
#include <stdio.h>
#include <stdlib.h>

typedef struct Cellule {
    int info;
    struct Cellule* suiv;
} Maillon;

// Fonction pour insérer un élément en tête de liste.
Maillon* inserer(int nombre, Maillon* liste) {
    Maillon* nouveau_maillon = (Maillon*)malloc(sizeof(Maillon));

    if (nouveau_maillon != NULL) {
        nouveau_maillon->info = nombre;
        nouveau_maillon->suiv = liste;

        return nouveau_maillon; // Le nouveau maillon devient la nouvelle tête de liste.
    }
}

int main() {
    Maillon* tete = NULL; // Initialisation de la liste vide.

    // Insérer des éléments en tête de liste.
    tete = inserer(10, tete);
    tete = inserer(20, tete);
    tete = inserer(30, tete);

    // Afficher les éléments de la liste.
    Maillon* courant = tete;
    while (courant != NULL) {
        printf("%d -> ", courant->info);
        courant = courant->suiv;
    }
    printf("NULL\n");

    // N'oubliez pas de libérer la mémoire lorsque vous avez terminé.
    courant = tete;
    while (courant != NULL) {
        Maillon* suivant = courant->suiv;
        free(courant);
        courant = suivant;
    }

    return 0;
}
// -- Fin du code à testée

b)
`
void sauvegarde(Maillon* liste,const char* fichier_name){
	FILE* fichier = fopen(fichier_name,"w");
	if(fichier!=NULL){
	    Maillon* courant = liste;
	    while(courant!=NULL){
	        fprintf(fichier,"%d\n",courant->info);
	        courant = courant-suiv;
	    }
	}
	fclose(fichier);
}
`
// -- Pour tester
#include <stdio.h>
#include <stdlib.h>

typedef struct Cellule {
    int info;
    struct Cellule* suiv;
} Maillon;

// Fonction pour insérer un élément en tête de liste.
Maillon* inserer(int nombre, Maillon* liste) {
    Maillon* nouveau_maillon = (Maillon*)malloc(sizeof(Maillon));
    if (nouveau_maillon != NULL) {
        nouveau_maillon->info = nombre;
        nouveau_maillon->suiv = liste;

        return nouveau_maillon; // Le nouveau maillon devient la nouvelle tête de liste.
    }
}

// Fonction pour sauvegarder la liste dans un fichier texte.
void sauvegarder_liste(const char* nom_fichier, Maillon* liste) {
    FILE* fichier = fopen(nom_fichier, "w");

    if (fichier != NULL) {
        Maillon* courant = liste;
        while (courant != NULL) {
            fprintf(fichier, "%d\n", courant->info);
            courant = courant->suiv;
        }
    }
    fclose(fichier);
}

int main() {
    Maillon* tete = NULL; // Initialisation de la liste vide.

    // Insérer des éléments en tête de liste.
    tete = inserer(10, tete);
    tete = inserer(20, tete);
    tete = inserer(30, tete);

    sauvegarder_liste("liste.txt", tete);

    // N'oubliez pas de libérer la mémoire lorsque vous avez terminé.
    Maillon *courant = tete;
    while (courant != NULL) {
        Maillon* suivant = courant->suiv;
        free(courant);
        courant = suivant;
    }

    return 0;
}
// -- Fin du code à testée

*/
