// @outhor : oholo.moulo23@inphb.ci
/*
Activité 6 :
Cette activité sera réalisée dans un premier temps en ligne de commande, puis dans un IDE.
Soit D:/Java/TP, votre répertoire de travail dans lequel se trouveront tous 
vos programmes Java. Il s'agit des programmes Point.java, Fenetre.java et testfenetre. java.
Le Programme Point.java contient Ia classe Point et le programme Fenetre.java
contient Ia classe Fenetre. Les deux classes sont dans le même package nommé
ci.inphb.Larima dont la racine est D:/java/packs. Autrement dit, aprės 
compilation, les bytecodes des deux classes se trouveront dans le package indiqué.
Le programme testFenetre.java contient la classe TestFenetre et dispose de
la méthode principale main().
NB:La racine des packages dans l'IDE Eclipse est le répertoire workspace par défaut.

Travail à faire:
Un objet de classe Point est caractérisé par ses coordonnées (abscisse et ordonné). En plus
de deux constructeurs, les méthodes de la classe Point sont:
- void deplacer (int x, int y): modifie les coordonnées du point
- void afficher(): affiche les coordonnées du point
Un objet de la classe Fenêtre est caractérisée par sa position qui est 
un objet de la classe Point, sa largeur et sa longueur. Ses méthodes sont:
- void deplacer (Point p): deplacer la fenêtre à un autre point
- void afficher(): afficher la position, la largeur et la longueur de la fenêtre.
La classe testFenetre crée un Objet Fenetre dans la methode principale main() 
et appelle la methode afficher() avec l'objet créé.
Ecrire toutes les classes, les compiler et tester la classe testFenetre
*/


import ci.inphb.Larima.*;

public class testFenetre {
    public static void main(String[] args) {
        Point p = new Point(5, 10);
        Fenetre f = new Fenetre(p, 20, 30);
        f.afficher();
    }
}

// Execution code :
// javac -d ../packs Point.java
// javac -d ../packs -cp ../packs Fenetre.java
// javac -cp ../packs  testFenetre.java
// java -cp "../packs:." testFenetre
