/*
Exercice java : Activité 3 (page 44)
Dans cette activité, on souhaite réaliser quelques opérations sur une matrice (tableau à deux dimensions) de N lignes et M colonnes. La matrice sera représentée par une classe Matrice qui va encapsuler un tableau à deux dimensions et les différents dimensions.
Un objet matrice sera créé, soit à partir d'un tableau primitif à deux dimensions ou à partir d'un objet matrice existant.
Ecrire la classe Matrice dotée des méthodes suivantes : 
- Constructeurs
- Affichage des éléments de la matrice
- Addition de deux matrices (le résultat est la matrice courante)
- Le produit de deux matrices (le résultat est la matrice courante)
*/


class Matrice {
  int[][] data;

  // Constructeur à partir d'un tableau primitif à deux dimensions
  Matrice(int[][] data) {
    this.data = new int[data.length][data[0].length];
    for (int i=0; i<data.length; i++) {
      for (int j=0; j<data[i].length; j++) {
        this.data[i][j] =data[i][j];
      }
    }
  }

    // Constructeur à partir d'un objet matrice existant
  Matrice(Matrice m) {
    data=new int[m.data.length][m.data[0].length];
    for (int i=0; i<m.data.length; i++) {
      for (int j=0; j<m.data[i].length; j++) {
        data[i][j]=m.data[i][j];
      }
    }
  }

  // Affichage des éléments de la matrice
  void afficher() {
    for (int[] row : data) {
      for (int val : row) { System.out.print(val + " "); }
      System.out.println();
    }
  }

  // Addition de deux matrices
  void addition(Matrice m) {
    if (data.length != m.data.length || data[0].length != m.data[0].length) {
      System.out.println("Les matrices doivent avoir les mêmes dimensions pour l'addition.");
      return;
    }

    for (int i = 0; i < data.length; i++) {
      for (int j = 0; j < data[i].length; j++) {
        data[i][j] += m.data[i][j];
      }
    }
  }

  // Produit de deux matrices
  void produit(Matrice m) {
    if (data[0].length != m.data.length) {
      System.out.println("Le nombre de colonnes de la première matrice doit être égal au nombre de lignes de la deuxième matrice pour le produit.");
      return;
    }
    int[][] result = new int[data.length][m.data[0].length];
    for (int i = 0; i < data.length; i++) {
      for (int j = 0; j < m.data[0].length; j++) {
        for (int k = 0; k < data[0].length; k++) {
          result[i][j] += data[i][k] * m.data[k][j];
        }
      }
    }
    data = result;
  }


  public static void main(String[] args) {
    // Création de deux matrices à partir de tableaux primitifs à deux dimensions
    int[][] data1 = {{1, 2}, {3, 4}}, data2 = {{5, 6}, {7, 8}};
    Matrice m1 = new Matrice(data1);
    Matrice m2 = new Matrice(new Matrice(data2));

    // Affichage des matrices
    System.out.println("Matrice 1 :");
    m1.afficher();
    System.out.println("Matrice 2 :");
    m2.afficher();

    // Addition de deux matrices
    System.out.println("Addition de Matrice 1 et Matrice 2 :");
    m1.addition(m2);
    m1.afficher();

    // Produit de deux matrices
    System.out.println("Produit de Matrice 1 et Matrice 2 :");
    m1 = new Matrice(data1);  // Réinitialisation de m1
    m1.produit(m2);
    m1.afficher();
    }
}

// Execution code : javac Matrice.java && java Matrice
