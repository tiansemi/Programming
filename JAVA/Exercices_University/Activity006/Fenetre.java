package ci.inphb.Larima;

public class Fenetre {
    private Point position;
    private int largeur;
    private int longueur;

    public Fenetre(Point position, int largeur, int longueur) {
        this.position = position;
        this.largeur = largeur;
        this.longueur = longueur;
    }

    public void deplacer(Point p) {
        this.position = p;
    }

    public void afficher() {
        System.out.println("Fenetre Position: ");
        position.afficher();
        System.out.println("Largeur: " + largeur + ", Longueur: " + longueur);
    }
}
