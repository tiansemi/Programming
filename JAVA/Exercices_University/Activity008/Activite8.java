// @outhor : oholo.moulo23@inphb.ci
/*
Activité 8 :
Une classe dispose d'une méthode paramétrée par un tableau de chaine de caractères.
En principe chaque chaine devrait être formée de chifres de telle sorte que
la méthode retourne la moyenne des éléments du tableau. Mais il se peut que
certaines chaines contiennent des caractères. Pour cela, on utilisera la
fonction Integer.parseInt(String s) qui convertit une chaine de caractère en
un nombre entier, si cela est possible; Dans le cas où la conversion n'est pas
possible une exception de type NumberFormatException est générée.

1) Ecrire et tester la classe Java correspondante. La moyenne des autres
éléments est toujours calculée, même au cas où des conversions sont impossibles.
::Solution::
public class Activite8 {
    public static double calculerMoyenne(String[] tableau) {
        double somme = 0, compteur = 0;
        for (String s : tableau) {
            try {
                int nombre = Integer.parseInt(s);
                somme += nombre;
                compteur++;
            } catch (NumberFormatException e) {
                System.out.println("Impossible de convertir '" + s + "' en nombre. Ignoré dans le calcul de la moyenne.");
            }
        }
        return compteur > 0 ? somme / compteur : 0;
    }

    public static void main(String[] args) {
        String tab[] = {"1","2","3","a","4"};
        double moyenne = calculerMoyenne(tab);
        System.out.println("La moyenne est : " + moyenne);
    }
}

// Execution code : javac Activite8.java && java Activite8

2) Transformer cette classe pour que les éléments du tableau puissent être entrés
en ligne de commande.
::Solution::

public class Activite8 {
    public static double calculerMoyenne(String[] tableau) {
        double somme = 0, compteur = 0;
        for (String s : tableau) {
            try {
                int nombre = Integer.parseInt(s);
                somme += nombre;
                compteur++;
            } catch (NumberFormatException e) {
                System.out.println("Impossible de convertir '" + s + "' en nombre. Ignoré dans le calcul de la moyenne.");
            }
        }
        return compteur > 0 ? somme / compteur : 0;
    }

    public static void main(String[] args) {
        double moyenne = calculerMoyenne(args);
        System.out.println("La moyenne est : " + moyenne);
    }
}

// Execution code : javac Activite8.java && java Activite8 1 2 3 a 4

*/

public class Activite8 {
    public static double calculerMoyenne(String[] tableau) {
        double somme = 0, compteur = 0;
        for (String s : tableau) {
            try {
                int nombre = Integer.parseInt(s);
                somme += nombre;
                compteur++;
            } catch (NumberFormatException e) {
                System.out.println("Impossible de convertir '" + s + "' en nombre. Ignoré dans le calcul de la moyenne.");
            }
        }
        return compteur > 0 ? somme / compteur : 0;
    }

    public static void main(String[] args) {
        double moyenne = calculerMoyenne(args);
        System.out.println("La moyenne est : " + moyenne);
    }
}

// Execution code : javac Activite8.java && java Activite8 10 2 355 a klk
