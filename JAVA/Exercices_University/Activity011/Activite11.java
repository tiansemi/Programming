// @author : oholo.moulo23@inphb.ci
/*
Activité 11
1) Ecrire un programme qui permet de saisir des chaines de caractères au clavier et les écrit dans un fichier
2) Ecrire un programme qui lit des chaines de caractères à partir d'un fichier et les les affcher afficher à l'écran.
Il faut impérativement utiliser les méthode read et write.
Quelques méthodes utiles de conversion de chaine de caractères en byte et vis versa
String(byte[] bytes)
String(byte[] bytes, int offset, int length)
string.getBytes()

1) Ecrire un programme qui permet de saisir des chaines de caractères au clavier et les écrit dans un fichier
::Solution::
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Scanner;

public class Activite11 {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Veuillez entrer une chaîne de caractères :");
        String input = sc.nextLine();

        try (FileOutputStream fos = new FileOutputStream("output.txt")) {
            byte[] bytes = input.getBytes();
            fos.write(bytes);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

// Execution code : javac Activite11.java && java Activite11

2) Ecrire un programme qui lit des chaines de caractères à partir d'un fichier et les les affcher afficher à l'écran.
::Solution::

import java.io.FileInputStream;
import java.io.IOException;

public class Activite11 {
    public static void main(String[] args) {
        try (FileInputStream fis = new FileInputStream("output.txt")) {
            byte[] bytes = new byte[fis.available()];
            int length;
            while ((length = fis.read(bytes)) != -1) {
                String content = new String(bytes, 0, length);
                System.out.println(content);
            }
            // ou utiliser 
            // byte[] bytes = fis.readAllBytes(); // Pour lire tout le contenu du fichier avec plusieurs lignes
            // String content = new String(bytes);
            // System.out.println(content);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

// Execution code : javac Activite11.java && java Activite11


NB : En Java, il est important de fermer les flux pour libérer les ressources qu'ils utilisent. Cependant, dans les exemples de code que j'ai fournis, j'ai utilisé une structure appelée "try-with-resources" qui est disponible à partir de Java 7.
La structure try-with-resources garantit que chaque ressource sera fermée à la fin de la déclaration. C'est pourquoi vous ne voyez pas de méthode `close()` explicite dans le code. Lorsque le bloc `try` se termine, toutes les ressources initialisées dans la déclaration seront fermées automatiquement, qu'il y ait une exception ou non.
Voici comment cela fonctionne dans le code :
```java
try (FileInputStream fis = new FileInputStream("nomFichier.txt")) {
    // Utilisez fis ici
} catch (IOException e) {
    // Gérez l'exception ici
}
// À ce stade, fis est automatiquement fermé, que le bloc try ait réussi ou qu'une exception ait été levée.
```
C'est une pratique recommandée car elle rend le code plus lisible et sûr. Il n'est pas nécessaire de se rappeler de fermer chaque flux dans un bloc `finally`, ce qui peut être source d'erreurs. De plus, cela garantit que les ressources sont fermées dans tous les cas, même en cas d'exceptions. C'est pourquoi vous ne voyez pas de `fis.close()` dans le code. Le flux `fis` est fermé automatiquement par la structure try-with-resources.
*/

import java.io.FileInputStream;
import java.io.IOException;

public class Activite11 {
    public static void main(String[] args) {
        try (FileInputStream fis = new FileInputStream("output.txt")) {
            byte[] bytes = new byte[fis.available()];
            int length;
            while ((length = fis.read(bytes)) != -1) {
                String content = new String(bytes, 0, length);
                System.out.println(content);
            }
            // ou utiliser 
            // byte[] bytes = fis.readAllBytes(); // Pour lire tout le contenu du fichier avec plusieurs lignes
            // String content = new String(bytes);
            // System.out.println(content);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

// Execution code : javac Activite11.java && java Activite11
