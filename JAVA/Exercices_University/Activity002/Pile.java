/*
Exercice java : Activité 2
Une pile est une structure de donnée représentant un ensemble (séquence) 
d'élément qui fonctionne selon le principe de LIFO(Last In, First Out). 
Les opérations sur une pile sont généralement :
- Empliler un élément x au sommet de la pile
- Dépiler l'élément x qui se trouve au sommet de la pile
- Tester si la pile est vide
- Tester si la pile est pleine
- Afficher le contenu de la pile
Dans cette activité, on décide de représenter une pile d'entier par un 
tableau d'entier. Dans ce cas, sommet représente l'indice du dernier élément 
déposé dans le tableau. Avant d'ajouter un élément dans la pile, 
l'attribut sommet sera d'abord incrémenté. De même après avoir rétiré 
l'élément sommet sera du sommet de la pile, l'attribut sommet sera décrémenté.
Ecrire et tester la classe Pile.
*/


class Pile {
  int maxSize;
  int[] stackArray;
  int top;

  Pile(int s) {
    maxSize = s;
    stackArray = new int[maxSize];
    top = -1;
  }

  void push(int j) {
    if(isFull()){
      System.out.println("La pile est pleine.");
    }else {
      stackArray[++top] = j;
    }
  }

  int pop() {
    if(isEmpty()){
      System.out.println("La pile est vide.");
      return -1;
    }else {
      return stackArray[top--];
    }
  }

  boolean isEmpty() {
    return (top == -1);
  }

  boolean isFull() {
    return (top == maxSize - 1);
  }

  void display() {
    if(isEmpty()){
      System.out.println("La pile est vide.");
    } else {
      for(int i = top; i >= 0; i--){
        System.out.println(stackArray[i]);
      }
    }
  }

  public static void main(String[] args) {
    Pile p = new Pile(5);
    for (int i=1;i<=5 ;i++ ) {
      p.push(i);
    }
    p.display();
    if (p.isFull()) System.out.println("La pile est pleine.");
    p.pop();
    if (!p.isEmpty()) System.out.println("la pile n'est pas vide.");
  }
}

// Execution code : javac Pile.java && java Pile
