// @author : oholo.moulo23@inphb.ci
/*
Activité 16
Cette activité vise à mettre en pratique le mécanisme de sérialisation des objets.
1) Ecrire une classe Etudiant sérialisable, comprenant les attributs suivants:
- nom
- Prénom
- Age
- Statut (boursier ou pas)
En plus du constructeur, cette classe sera dotée d'une méthode <<void afficher()>> pour l'afichage d'un objet Etudiant.
2) Ecrire une classe dans laquelle un objet Etudiant est sérialisé dans un fichier, puis restauré.
Faire des tests en affichant l'objet Etudiant avant la sérialisation et après la restauration


1) Ecrire une classe Etudiant sérialisable
:Solution:
import java.io.Serializable;
class Etudiant implements Serializable {
    private String nom;
    private String prenom;
    private int age;
    private boolean statut;

    public Etudiant(String nom, String prenom, int age, boolean statut) {
        this.nom = nom;
        this.prenom = prenom;
        this.age = age;
        this.statut = statut;
    }

    public void afficher() {
        System.out.println("Nom: " + nom);
        System.out.println("Prénom: " + prenom);
        System.out.println("Age: " + age);
        System.out.println("Statut: " + (statut ? "Boursier" : "Non boursier"));
    }
}

2) Ecrire une classe dans laquelle un objet Etudiant est sérialisé dans un fichier, puis restauré.
import java.io.*;
public class TestEtudiant {
    public static void main(String[] args) {
        Etudiant etudiant = new Etudiant("Dupont", "Jean", 20, true);
        etudiant.afficher();

        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("etudiant.ser"))) {
            oos.writeObject(etudiant);
        } catch (IOException e) {
            e.printStackTrace();
        }

        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream("etudiant.ser"))) {
            Etudiant etudiantRestaure = (Etudiant) ois.readObject();
            etudiantRestaure.afficher();
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
    }
}
// Compilation et Execution : javac Activite16.java && java Activite16
// Vérification : type etudiant.ser [Sous Windows ou ] cat etudiant.ser [Sous Linux]

*/

import java.io.*;

class Etudiant implements Serializable {
    private String nom;
    private String prenom;
    private int age;
    private boolean statut;

    public Etudiant(String nom, String prenom, int age, boolean statut) {
        this.nom = nom;
        this.prenom = prenom;
        this.age = age;
        this.statut = statut;
    }

    public void afficher() {
        System.out.println("Nom: " + nom);
        System.out.println("Prénom: " + prenom);
        System.out.println("Age: " + age);
        System.out.println("Statut: " + (statut ? "Boursier" : "Non boursier"));
    }
}

public class Activite16 {
    public static void main(String[] args) {
        Etudiant etudiant = new Etudiant("Dupont", "Jean", 20, true);
        etudiant.afficher();

        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("etudiant.ser"))) {
            oos.writeObject(etudiant);
        } catch (IOException e) {
            e.printStackTrace();
        }

        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream("etudiant.ser"))) {
            Etudiant etudiantRestaure = (Etudiant) ois.readObject();
            etudiantRestaure.afficher();
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
    }
}
