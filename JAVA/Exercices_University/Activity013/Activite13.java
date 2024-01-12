// @author : oholo.moulo23@inphb.ci
/*
Activité 13
Ecrire et tester un programme qui permet copier le contenu d'un fichier dans un autre de deux manières:
1) Caractère par caractère
2) Bloc par bloc
NB: Les classes à utiliser sont uniquement FileReader et FileWriter
Les méthode à utiler sont:
int read() et void write(int c) pour caractère par caractère.
int read (char[]) et write(char[]) pour bloc par bloc


1) Caractère par caractère
:Solution:
import java.io.FileWriter;
import java.io.FileReader;
import java.io.IOException;

public class Activite13 {
    public static void main(String []args) {
        try (FileReader fread = new FileReader("nomFichier.txt") ; FileWriter fwrite = new FileWriter("output.txt")) {
            int c ;
            while ((c=fread.read()) != -1)
                fwrite.write(c);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
// javac Activite13.java && java Activite13

2) Bloc par bloc
:Solution:
import java.io.FileWriter;
import java.io.FileReader;
import java.io.IOException;

public class Activite13 {
    public static void main(String []args) {
        try (FileReader fread = new FileReader("nomFichier.txt") ; FileWriter fwrite = new FileWriter("output.txt")) {
            char[] buffer = new char[1024]; 
            int bytesRead;
            while ((bytesRead=fread.read(buffer)) != -1)
                fwrite.write(buffer, 0, bytesRead);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
// javac Activite13.java && java Activite13

*/


import java.io.FileWriter;
import java.io.FileReader;
import java.io.IOException;

public class Activite13 {
    public static void main(String []args) {
        try (FileReader fread = new FileReader("nomFichier.txt") ; FileWriter fwrite = new FileWriter("output.txt")) {
            char[] buffer = new char[1024]; 
            int bytesRead;
            while ((bytesRead=fread.read(buffer)) != -1)
                fwrite.write(buffer, 0, bytesRead);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
