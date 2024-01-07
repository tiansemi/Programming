/*
Activité 1:
L'objectif de cette activité est de créer et exécuter une classe en ligne de commande.
Le programme Java sera saisi dans un éditeur de texte (notepad, subblime, Ou autre).
Travail à faire
1) Compléter la classe Point avec deux constructeurs de l'exemple: 
un constructeur sans paramètre et un constructeur avec deux paramètres.
Dans le premier cas, les attibuts internes (X et y) seront initialisés à 0.
Dans le deuxième cas, les attibuts seront initialisés avec les paramétres du constructeur.
2) Créer un objet Point dans la méthode main() et appeler la méthode afficher()
3) Renseigner la variable d'environnement PATH avec le répertoire d'installation
du compilateur Java (javac.exe) et de la machine virtuelle Java (java.exe)
4) Compiler et lancer la classe créée.
*/
class Point{
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

// Solution pour 3): setx PATH "%PATH%;C:\Program Files\Java\jdk1.8.0_241\bin"
// Solution pour 4): javac Point.java && java Point
