#!/bin/bash

: "
@author : tiansemi@outlook.com
steps : 

$ mkdir webfonts && cd webfonts
$ chmod +x download.sh
$ ./autodownload.sh

NB : Ce script lit le fichier requirements.txt ligne par ligne, et chaque ligne est considérée comme le nom d’un fichier à télécharger. Assurez-vous que le fichier requirements.txt est dans le même répertoire que le script, ou modifiez le chemin du fichier en conséquence
Exemple de contenu du requirements.txt  :
fa-solid-900.eot
fa-solid-900.woff2
fa-solid-900.woff
fa-solid-900.ttf
fa-solid-900.svg
fa-regular-400.eot
fa-regular-400.woff2
fa-regular-400.woff
fa-regular-400.ttf
fa-regular-400.svg
fa-brands-400.eot
fa-brands-400.woff2
fa-brands-400.woff
fa-brands-400.ttf
fa-brands-400.svg 
"

# URL de base
base_url="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.13.0/webfonts/"

# Lire le fichier requirements.txt
while IFS= read -r file
do
  # Construction de l'URL du fichier
  file_url="${base_url}${file}"
  
  # Téléchargement du fichier
  wget $file_url
done < requirements.txt
