// @author : oholo.moulo23@inphb.ci
/*
Activité 14
1) Ecrire un programme qui permet de lire le contenu d'un fichier existant ligne par ligne en utilisant les classes
FileReader et BufferedReader
2) Ecrire un programme qui lit des lignes de caractères au clavier et les écrit dans un fichier, La classe
BufferedReader sera utilisé à la place de la classe Scanner.


1) Ecrire un programme qui permet de lire le contenu d'un fichier existant ligne par ligne en utilisant les classes FileReader et BufferedReader
:Solution:
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Activite14 {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("nomFichier.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                System.out.println(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
// javac Activite14.java && java Activite14

2) Ecrire un programme qui lit des lignes de caractères au clavier et les écrit dans un fichier, La classe BufferedReader sera utilisé à la place de la classe Scanner.
:Solution:
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;

public class Activite14 {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
             BufferedWriter bw = new BufferedWriter(new FileWriter("output.txt"))) {
            System.out.println("Saisissez vos lignes de texte : ");
            String input;
            while (!(input = br.readLine()).equals("")) {
                bw.write(input);
                bw.newLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}


*/

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;

public class Activite14 {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
             BufferedWriter bw = new BufferedWriter(new FileWriter("output.txt"))) {
            System.out.println("Saisissez vos lignes de texte : ");
            String input;
            while (!(input = br.readLine()).equals("")) {
                bw.write(input);
                bw.newLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
