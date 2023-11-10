/*

Enoncé : 
1)	L utilisateur «Balla» connecté à un système Linux se trouve dans son répertoire de travail (/home/balla).
Il désire copier tous les fichiers du répertoire «/media/usb» dans son répertoire de travail. La commande utilisée est «cp». 
Donner deux manières (commandes) de faire cette copie.
2)	Balla dispose de deux sous répertoires dans son répertoire de travail nommés respectivement «Sauve» et «TP». 
Il se positionne dans le sous répertoire «TP» et veut se déplacer dans le sous répertoire «Sauve». Donner deux manières (commandes) de faire ce déplacement.
3)	Etant donné un fichier nommé «Fixe», quels droits affecte-t-on à ce fichier via la commande : chmod 744 Fixe

Solution : 
1) les commandes sont :
$ cp -r /media/usb/ /home/balla/ 
$ cp /media/usb/* /home/balla/

2) les commandes sont : 
$ cd /home/balla/Sauve/
$ cd ../Sauve/

3) La commande `chmod 744 Fixe` attribue les droits 7 (lecture, écriture et exécution) à l'utilisateur propriétaire du fichier, 
les droits 4 (lecture) au groupe propriétaire, et les droits 4 (lecture) aux autres utilisateurs.

*/
