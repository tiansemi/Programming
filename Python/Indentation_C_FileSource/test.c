#include<stdio.h>
int i,j;
void main (){ scanf("%d %d", &i, &j);if(i>j){i=i-j;j=j+1;}if(i<0){j=i+j;i=2+j;}}
