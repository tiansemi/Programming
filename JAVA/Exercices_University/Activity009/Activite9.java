/*
Activité 9
Une classe contient trois tableaux de N entiers a, b et c. Cette classe dispose d'une méthode void remplir() qui permet de construire le
tableau c à partir des tableaux a et b avec c[i]=a[i]/b[i]. Les tableau a et b sont initialisés à la déclaration. Au moins un élément du tableau b est nul.
La fonction remplir est évoquée dans la méthode principale main(). La méthode remplir() ne traite pas les exceptions, mais les reporte au niveau supérieur. Ecrire le programme java en gérant les cas d'exception.
*/

public class Activite9 {
    static int[] a = {2, 4, 6, 8, 10};
    static int[] b = {1, 5, 2, 0, 5};
    private static int[] c = new int[a.length];

    public static void remplir(int i) throws ArithmeticException {
        c[i] = a[i] / b[i];
    }

    public static void main(String[] args) {
        for (int i = 0; i < a.length; i++) {
            try {
                remplir(i);
            } catch (ArithmeticException e) {
                c[i] = 0;
            }
        }
        // Afficher le tableau c
        for (int i = 0; i < c.length; i++) {
            System.out.print(c[i] + " ");
        }
    }
}


// Execution code : javac Activite9.java && java Activite9
