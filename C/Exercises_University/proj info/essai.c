
#include <stdio.h>
int main(int nbArg, char*arg[]){    printf("nom du programme:%s/n",arg[0]);for(int i=1;i<nbArg;i++)    printf ("Argument %d:%s/n" ,i, arg[i]);return 0;}
