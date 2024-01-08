package ci.inphb.Larima;

public class Point {
    private int x;
    private int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Point() {
        x=0;y=0;
    }

    public void deplacer(int X, int Y) {
        this.x = X;
        this.y = Y;
    }

    public void afficher() {
        System.out.println("Point(" + x + ", " + y + ")");
    }
}
