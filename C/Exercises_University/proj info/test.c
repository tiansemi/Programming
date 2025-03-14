#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int count_for(FILE *fp) {
    char line[22], ch;
    int count = 0;

    printf("\n\n-debut-\n");
    while ( (ch=fgetc(fp)) != '>' && ch != EOF) {
        // char *pos = strstr(line, "for(");
        // if (pos != NULL) {
        //     count++;
        // }
        printf("%c",ch );
    }
    if(ch=='>')    printf("%c", ch);
    printf("\n-fini-\n\n");

    return count;
}
// bool is_for_loop(FILE* input, int cursor) {
//     char buffer[23]; 
//     int i;

//     buffer[0] = 'f';
//     for (i = 1; i <= 22; i++) {
//         char ch = fgetc(input);
//         if (ch == EOF || ch == '\n') {
//             break;
//         }

//         buffer[i] = ch;
//     }

//     buffer[i] = '\0';

//     if (strncmp(buffer, "for(", 4) == 0) {
//         // Rembobiner le nombre de caractères lus pour ne pas perdre de données
//         fseek(input,-(i-1),SEEK_CUR);

//         // La chaîne commence par "for(" ; c'est une boucle for
//         return true;
//     }

//     fseek(input,-(i-1),SEEK_CUR);

//     return false;
// }


int main(int argc, char const *argv[])
{
    FILE *file = fopen("file.txt","r");
    char ch ;
    printf("__Text__ :\n\n");
    while( (ch=fgetc(file)) != EOF && ch != '{'){
        printf("%c",ch );
        if(ch == '>')   count_for(file);
    }
    // printf("%d\n", count_for("file.txt") );
    return 0;
}