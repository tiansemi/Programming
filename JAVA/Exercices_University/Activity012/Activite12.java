// @author : oholo.moulo23@inphb.ci
/*
Activité 12
1) Ecrire et tester un programme qui permet de lire des lignes de texte au clavier et les écrit dans
un fichier. La saisie s'arrête lorsqu'on rentre une ligne vide. La méthode equals(String s) de la classe String permet de comparer une chaine de caractère à une autre. Le décorateur PrintStream doit être utilisé.

:Solution:
import java.io.PrintStream;
import java.io.IOException;
import java.util.Scanner;

public class Activite12 {
    public static void main(String[] args) {
        try (PrintStream ps = new PrintStream("output.txt")) {
            Scanner sc = new Scanner(System.in);
            System.out.println("Saisissez vos lignes de texte : ");
            String input;
            while (!(input = sc.nextLine()).equals("")) {
                ps.println(input);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
// javac Activite12.java && java Activite12

2) Ecrire et tester un programme qui permet de lire des lignes de texte à partir d'un fichier et les
écrit à l'écran. Le décorateur Scanner doit être ulilisé pour la lecture de ligne de fichier.

:Solution:
import java.io.File;
import java.io.IOException;
import java.util.Scanner;

public class Activite12 {
    public static void main(String[] args) {
        try (Scanner sc = new Scanner(new File("output.txt"))) {
            while (sc.hasNextLine()) {
                System.out.println(sc.nextLine());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
// javac Activite12.java && java Activite12

3) Ecrire et tester un programme qui lit des lignes de fichier à partir d'un fichier et les écrit dans un autre fichier. Il s'agit d'une compilation des 2 questions précédentes.

:Solution:
import java.io.File;
import java.io.PrintStream;
import java.io.IOException;
import java.util.Scanner;

public class Activite12 {
    public static void main(String[] args) {
        try (Scanner sc = new Scanner(new File("output.txt"));
             PrintStream ps = new PrintStream("output2.txt")) {
            while (sc.hasNextLine()) {
                ps.println(sc.nextLine());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
// javac Activite12.java && java Activite12
*/



import java.io.File;
import java.io.PrintStream;
import java.io.IOException;
import java.util.Scanner;

public class Activite12 {
    public static void main(String[] args) {
        try (Scanner sc = new Scanner(new File("output.txt"));
             PrintStream ps = new PrintStream("output2.txt")) {
            while (sc.hasNextLine()) {
                ps.println(sc.nextLine());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
