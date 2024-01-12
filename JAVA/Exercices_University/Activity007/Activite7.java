// @outhor : oholo.moulo23@inphb.ci
/*
Activité 7
Soient trois tableaux de N entiers: a, b et c. 
Le tableau c est construit à partir des tableaux a et b avec c[i] = a[i]/b[i]. 
Les tableau a et b sont initialisés à la déclaration. 
Au moins un élément du tableau b est nul (0).
Ecrire le programme Java pour construire le tableau c, 
en gérant les cas d'exception. On considera que lorsque b[i] vaut 0, 
c[i] reçoit 0.
*/

public class Activite7 {

    public static void main(String[] args) {
        // Déclarer et initialiser les tableaux a et b
        int[] a = {2, 4, 6, 8, 10};
        int[] b = {1, 0, 2, 0, 5};
        // Déclarer le tableau c de même taille que a et b
        int[] c = new int[a.length];
        // Parcourir les tableaux a et b
        for (int i = 0; i < a.length; i++) {
            // Essayer de calculer c[i] = a[i] / b[i]
            try {
                c[i] = a[i] / b[i];
            }
            // Attraper l'exception de division par zéro
            catch (ArithmeticException e) {
                // Affecter 0 à c[i] si b[i] vaut 0
                c[i] = 0;
            }
        }
        // Afficher le tableau c
        for (int i = 0; i < c.length; i++) {
            System.out.print(c[i] + " ");
        }
    }
}

// Execution code : javac Activite7.java && java Activite7
