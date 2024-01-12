// @author : oholo.moulo23@inphb.ci
/*
Activité 5
L'objectif de cet atelier est de mettre en œuvre les concepts de polymorphisme, de classe abstraite et d'interface.
On suppose qu'une forme géométrique est une figure dont on peut calculer le périmètre et la surface. Une forme 
géométrique possėde une position (objet de la classe Point) dans le plan. Le triangle, le cercle , le carré, 
le reciangle sont des exemples de formes géométrique.
1) Ecrire la classe abstraite nommé FormeGeo représentant les formes géomėtrique. Elle sera doté des méthodes 
abstraites float surface() et float périmètre() qui renvoient respectivement la surface et le périmètre de la forme géométrique.
2) Ecrire les classes Rectangle et Cercle qui redéfinissent les méthodes précédentes
3) Un domaine est un ensemble de forme géométrique. La surface d'un domaine est la somme des surfaces des formes 
géométriques qui le composent. En supposant que le domaine est un tableau de forme géométriques, écrire la classe 
Domaine dotée de la méthode float surface().
4) Reprendre les question 1,2 et 3 en faisant de la classe abstraite formeGeo, une interface.


1) Ecrire la classe abstraite nommé FormeGeo représentant les formes géomėtrique.
:Solution:
abstract class FormeGeo {
    Point position;
    FormeGeo(Point p){
        position.x = p.x; position.y = p.y;
    }
    public abstract float surface();
    public abstract float perimetre();
}

2) Ecrire les classes Rectangle et Cercle qui redéfinissent les méthodes précédentes
:Solution:
class Rectangle extends FormeGeo {
    private float longueur;
    private float largeur;

    public Rectangle(float longueur, float largeur) {
        this.longueur = longueur;
        this.largeur = largeur;
    }
    @Override
    public float surface() {  return longueur * largeur;  }
    @Override
    public float perimetre() {  return 2 * (longueur + largeur); }
}

class Cercle extends FormeGeo {
    private float rayon;

    public Cercle(float rayon) { this.rayon = rayon; }
    @Override
    public float surface() {  return (float) Math.PI * rayon * rayon; }
    @Override
    public float perimetre() {  return 2 * (float) Math.PI * rayon; }
}


3) Un domaine est un ensemble de forme géométrique. La surface d'un domaine est la somme des surfaces des formes géométriques qui le composent. En supposant que le domaine est un tableau de forme géométriques, écrire la classe Domaine dotée de la méthode float surface().
:Solution:
class Domaine {
    private FormeGeo[] formes;
    public Domaine(int Taille) { formes = new FormeGeo[Taille]; }

    public float surface() {
        float surfaceTotale = 0;
        for (FormeGeo forme : formes) { surfaceTotale += forme.surface(); }
        return surfaceTotale;
    }
}


// Test code Complet (Execution : javac Activite5.java && java Activite5) : 
// "Domaine" a été remplacé par "Activite5" pour permettre l'exécution du code dans ce même fichier.
class Point{ // pris depuis : https://github.com/tiansemi/Programming/blob/main/JAVA/Exercices_University/Activity001/Point.java
  int x,y;
  Point(){x=0;y=0;};
  Point(int X, int Y){x=X;y=Y;};
  int obtenirAbs() {return x;};
  int obtenirOrd() {return y;};
  void afficher(){ System.out.println("Abscisse = "+x+" Ordonnee = "+y); };
  public String toString(){ return "Abscisse = "+x+" Ordonnee = "+y; }
  public Object clone() throws CloneNotSupportedException{
    return new Point(this.x, this.y);
  }
  public boolean equals(Object obj){
    Point p = (Point) obj;
    if(p.x==this.x && p.y==this.y){ return true;}
    else{ return false;}
  }

  public static void main(String []args){
    Point point2 = new Point(10,20);
    point2.afficher();
    Point point1=point2;
    System.out.println(point1);
    System.out.println(point2==point1);
  }
}

abstract class FormeGeo {
    Point position = new Point();
    FormeGeo(Point p){
        position.x = p.x; position.y = p.y;
    }
    public abstract float surface();
    public abstract float perimetre();
}

class Rectangle extends FormeGeo {
    private float longueur;
    private float largeur;

    public Rectangle(float longueur, float largeur, Point position) {
        super(position);
        this.longueur = longueur;
        this.largeur = largeur;
    }
    @Override
    public float surface() {  return longueur * largeur;  }
    @Override
    public float perimetre() {  return 2 * (longueur + largeur); }
}

class Cercle extends FormeGeo {
    private float rayon;

    public Cercle(float rayon, Point position) {
        super(position); 
        this.rayon = rayon; 
    }
    @Override
    public float surface() {  return (float) Math.PI * rayon * rayon; }
    @Override
    public float perimetre() {  return 2 * (float) Math.PI * rayon; }
}

public class Activite5 {
    private FormeGeo[] formes;
    public Activite5(int Taille) { formes = new FormeGeo[Taille]; }

    public float surface() {
        float surfaceTotale = 0;
        for (FormeGeo forme : formes) { surfaceTotale += forme.surface(); }
        return surfaceTotale;
    }

    public static void main(String[] args) {
        Activite5 domaine = new Activite5(2);
        domaine.formes[0] = new Rectangle(5, 10, new Point(0, 0));
        domaine.formes[1] = new Cercle(7, new Point(0, 0));
        System.out.println("La surface totale du domaine est : " + domaine.surface());
    }
}


4) Reprendre les question 1,2 et 3 en faisant de la classe abstraite formeGeo, une interface.
:Solution:
interface FormeGeo {
    float surface();
    float perimetre();
}

class Rectangle implements FormeGeo {
    private Point position;
    private float longueur;
    private float largeur;

    public Rectangle(Point position, float longueur, float largeur) {
        this.position = position;
        this.longueur = longueur;
        this.largeur = largeur;
    }

    @Override
    public float surface() {
        return longueur * largeur;
    }

    @Override
    public float perimetre() {
        return 2 * (longueur + largeur);
    }
}

class Cercle implements FormeGeo {
    private Point position;
    private float rayon;

    public Cercle(Point position, float rayon) {
        this.position = position;
        this.rayon = rayon;
    }

    @Override
    public float surface() {
        return (float) Math.PI * rayon * rayon;
    }

    @Override
    public float perimetre() {
        return 2 * (float) Math.PI * rayon;
    }
}

public class Domaine {
    private FormeGeo[] formes;

    public Domaine(int taille) {
        formes = new FormeGeo[taille];
    }

    public float surface() {
        float surfaceTotale = 0;
        for (FormeGeo forme : formes) {
            surfaceTotale += forme.surface();
        }
        return surfaceTotale;
    }

    public static void main(String[] args) {
        Domaine domaine = new Domaine(2);
        domaine.formes[0] = new Rectangle(new Point(0, 0), 5, 10);
        domaine.formes[1] = new Cercle(new Point(0, 0), 7);
        System.out.println("La surface totale du domaine est : " + domaine.surface());
    }
}
javac Domaine.java && java Domaine

*/
interface FormeGeo {
    float surface();
    float perimetre();
}

class Rectangle implements FormeGeo {
    private Point position;
    private float longueur;
    private float largeur;

    public Rectangle(Point position, float longueur, float largeur) {
        this.position = position;
        this.longueur = longueur;
        this.largeur = largeur;
    }

    @Override
    public float surface() {
        return longueur * largeur;
    }

    @Override
    public float perimetre() {
        return 2 * (longueur + largeur);
    }
}

class Cercle implements FormeGeo {
    private Point position;
    private float rayon;

    public Cercle(Point position, float rayon) {
        this.position = position;
        this.rayon = rayon;
    }

    @Override
    public float surface() {
        return (float) Math.PI * rayon * rayon;
    }

    @Override
    public float perimetre() {
        return 2 * (float) Math.PI * rayon;
    }
}

public class Activite5 {
    private FormeGeo[] formes;

    public Activite5(int taille) {
        formes = new FormeGeo[taille];
    }

    public float surface() {
        float surfaceTotale = 0;
        for (FormeGeo forme : formes) {
            surfaceTotale += forme.surface();
        }
        return surfaceTotale;
    }

    public static void main(String[] args) {
        Activite5 domaine = new Activite5(2);
        domaine.formes[0] = new Rectangle(new Point(0, 0), 5, 10);
        domaine.formes[1] = new Cercle(new Point(0, 0), 7);
        System.out.println("La surface totale du domaine est : " + domaine.surface());
    }
}
