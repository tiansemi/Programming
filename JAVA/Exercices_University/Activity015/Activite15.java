// @author : oholo.moulo23@inphb.ci
/*
Activité 15
1) Ecrire un code qui affiche la liste des répertoires racine d'un système de fichiers (C:\,D:\, E:\,...)
2) Ecrire un code qui affiche les noms des fichiers et des répertoires du répertoire courant représenté par <<.>>
3) Ecrire un programme qui saisit le nom d'un répertoire au clavier et afiche le type de chacun des éléments (fichier ou répertoire)


1) Ecrire un code qui affiche la liste des répertoires racine d'un système de fichiers (C:\,D:\, E:\,...)
:Solution:
import java.io.File;

public class Activite15 {
    public static void main(String[] args) {
        for (File root : File.listRoots())
            System.out.println(root);
    }
}
// javac Activite15.java && java Activite15

2) Ecrire un code qui affiche les noms des fichiers et des répertoires du répertoire courant représenté par <<.>>
:Solution:
import java.io.File;

public class Activite15 {
    public static void main(String[] args) {
        File dir = new File(".");
        String[] children = dir.list();
        if (children != null) {
            for (String child : children)
                System.out.println(child);
        }
    }
}
// javac Activite15.java && java Activite15

3) Ecrire un programme qui saisit le nom d'un répertoire au clavier et afiche le type de chacun des éléments (fichier ou répertoire)
:Solution:
import java.io.File;
import java.util.Scanner;

public class Activite15 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Veuillez entrer le nom d'un répertoire :");
        String dirName = scanner.nextLine();

        File dir = new File(dirName);
        File[] files = dir.listFiles();
        if (files != null) {
            for (File file : files) {
                if (file.isDirectory()) {
                    System.out.println(file.getName() + " est un répertoire.");
                } else {
                    System.out.println(file.getName() + " est un fichier.");
                }
            }
        }
    }
}
// javac Activite15.java && java Activite15

*/



import java.io.File;
import java.util.Scanner;

public class Activite15 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Veuillez entrer le nom d'un répertoire :");
        String dirName = scanner.nextLine();

        File dir = new File(dirName);
        File[] files = dir.listFiles();
        if (files != null) {
            for (File file : files) {
                if (file.isDirectory()) {
                    System.out.println(file.getName() + " est un répertoire.");
                } else {
                    System.out.println(file.getName() + " est un fichier.");
                }
            }
        }
    }
}
