import ci.inphb.Larima.*;

public class testFenetre {
    public static void main(String[] args) {
        Point p = new Point(5, 10);
        Fenetre f = new Fenetre(p, 20, 30);
        f.afficher();
    }
}

// Execution code :
// javac -d ../packs Point.java
// javac -d ../packs -cp ../packs Fenetre.java
// javac -cp ../packs  testFenetre.java
// java -cp "../packs:." testFenetre
