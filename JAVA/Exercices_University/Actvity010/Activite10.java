// @author : oholo.moulo23@inphb.ci
/*
Activité 10
Dans cette activitė on souhaite manipuler les flux d'entrée-sortie.
1. Un objet flux de la classe de flux de fichier en entrée (FileInputstream) peut étre associé a un fichier physique au moment de la création de cet objet en uliisant le constructeur FileInputStream(String nomFicher) throws FileNotFoundException;
a) Donner l'instruclion qui permet l'association du flux à un fichier physique
b ) Ecrire un programme qui permet de lire le contenu d'un fichier, caractère par caractère, et l'affche à l'écran
2.a) Reprendre la question 1.a) pour le flux FileOutputStream dont le constructeur est FileOutputStream(String nomFichier) throws FileNotFoundException;
b) Ecrire un programme qui permet d'écrire des caractères saisis au clavier dans un fichier. La saisie s'arrête à la saisie d'une ligne vide . Il faut noter que la classe Scanner ne prévoit pas la lecture direct de caractère. Pour cela, Il faut lire d'abord une chaine de caractère, puis extraire le premier caractère de cette chaine en utilisant la méthode charAt(int index) de la classe String.


1.a) Donner l'instruction qui permet l'association du flux à un fichier physique
:Solution:
FileInputStream fis = new FileInputStream("nomDuFichier.ext");

1.b ) Ecrire un programme qui permet de lire le contenu d'un fichier, caractère par caractère, et l'affche à l'écran
import java.io.FileInputStream;
import java.io.IOException;

public class Activite10 {
    public static void main(String[] args) {
        try (FileInputStream fis = new FileInputStream("nomFichier.txt")) {
            int character;
            while ((character = fis.read()) != -1) {
                System.out.print((char) character);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

2.a) Reprendre la question 1.a) pour le flux FileOutputStream dont le constructeur est FileOutputStream(String nomFichier) throws FileNotFoundException;
FileOutputStream fos = new FileOutputStream("nomFichier.txt");

2.b) Ecrire un programme qui permet d'écrire des caractères saisis au clavier dans un fichier. La saisie s'arrête à la saisie d'une ligne vide . Il faut noter que la classe Scanner ne prévoit pas la lecture direct de caractère. Pour cela, Il faut lire d'abord une chaine de caractère, puis extraire le premier caractère de cette chaine en utilisant la méthode charAt(int index) de la classe String.
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Scanner;

public class Activite10 {
    public static void main(String[] args) {
        try (FileOutputStream fos = new FileOutputStream("nomFichier.txt")) {
            Scanner sc = new Scanner(System.in);
            System.out.println("Saisissez vos caractères : ");
            String input;
            while (!(input = sc.nextLine()).isEmpty()) {
                for(int i=0; i<input.length(); i++)
                    fos.write(input.charAt(i));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
// javac Activite10.java && java Activite10
*/


import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Scanner;

public class Activite10 {
    public static void main(String[] args) {
        try (FileOutputStream fos = new FileOutputStream("nomFichier.txt")) {
            Scanner sc = new Scanner(System.in);
            System.out.println("Saisissez vos caractères : ");
            String input;
            while (!(input = sc.nextLine()).isEmpty()) {
                for(int i=0; i<input.length(); i++)
                    fos.write(input.charAt(i));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

