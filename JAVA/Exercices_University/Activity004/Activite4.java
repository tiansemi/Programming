// @outhor : oholo.moulo23@inphb.ci
/*
Activité 4
Dans cette activité, on se propose d'implémenter une classe Pile d'entier par réutilisation d'une liste linéaire chainée 
d'entiers. Une liste chainée est une structure de données telle que la connaissance d'un élément permet de connaitre
l'élément suivant. En effet, l'élément courant contient, en plus de la donnée, la référence vers l'élément suivant, Le 
dernier élément de la liste contient une référence nulle pour l'élèment suivant.
On peut insérer ou retirer un élément en début de liste, en fin de liste, à n'importe quelle position de la liste.
Chaque élément de la liste contient deux champs: un nombre et la référence vers l'élément suivant. Un élément sera donc 
représenté par une classe ayant les deux attributs et un constructeur comme seule méthode.
L'élément de la liste est implémentée par la classe Cellule :
class Cellule {
    int info;
    Cellule suiv;
    Cellule(int info, Cellule suiv) {
        this.info=info;
        this.suiv=sulv;
    }
}
1) Une liste est définie par la donnée de son premier élément. Ecrire la classe Liste avec les méthodes d'insertion en 
tête de liste, d'insertion en fin de liste, et de retrait du premier élément de la liste
2) Ecrire une classe Pile qui hérite de la classe Liste.



1) Une liste est définie par la donnée de son premier élément. Ecrire la classe Liste avec les méthodes d'insertion en tête de liste, d'insertion en fin de liste, et de retrait du premier élément de la liste
:Solution:
class Liste {
    Cellule premier;
    public Liste() { premier=null;}
    public Liste(Cellule T) {  premier = new Cellule(T.info, null); }
    void insererEnTete(int info) { premier = new Cellule(info, premier); }

    void insererEnFin(int info) {
        if (premier == null) {
            premier = new Cellule(info, null);
        } else {
            Cellule temp = premier;
            while (temp.suivant != null)
                temp = temp.suivant;
            temp.suivant = new Cellule(info, null);
        }
    }

    int retirerPremier() {
        if (premier != null) {
            int t = premier.info;
            premier = premier.suivant;
            return t;
        }else
            return 99999;
    }
}


2) Ecrire une classe Pile qui hérite de la classe Liste.
:Solution:
class Pile extends Liste {
    void empiler(int info) {
        insererEnTete(info);
    }

    void depiler() {
        retirerPremier();
    }
}
*/


class Cellule {
    int info;
    Cellule suivant;

    Cellule(int info, Cellule suivant) {
        this.info = info;
        this.suivant = suivant;
    }
}

class Liste {
    Cellule premier;
    public Liste() { premier=null;}
    public Liste(Cellule T) {  premier = new Cellule(T.info, null); }
    void insererEnTete(int info) { premier = new Cellule(info, premier); }

    void insererEnFin(int info) {
        if (premier == null) {
            premier = new Cellule(info, null);
        } else {
            Cellule temp = premier;
            while (temp.suivant != null)
                temp = temp.suivant;
            temp.suivant = new Cellule(info, null);
        }
    }

    int retirerPremier() {
        if (premier != null) {
            int t = premier.info;
            premier = premier.suivant;
            return t;
        }else
            return 99999;
    }
}

class Pile extends Liste {
    void empiler(int info) {
        insererEnTete(info);
    }

    void depiler() {
        retirerPremier();
    }
}

public class Activite4 {
    public static void main(String[] args) {
        Liste liste = new Liste();
        liste.insererEnTete(18);
        liste.insererEnFin(60);
        liste.insererEnFin(125);
        liste.insererEnFin(18);
        liste.retirerPremier();

        Cellule temp = liste.premier;
        while (temp != null) {
            System.out.println(temp.info);
            temp = temp.suivant;
        }
    }
}

// Compilation et Execution :  javac Activite4.java && java Activite4
