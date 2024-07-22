#!/bin/bash

# Vérifie si compare_kdbx existe dans /usr/local/bin
if [ -f /usr/local/bin/compare_kdbx ]; then
    # Supprime le fichier compare_kdbx
    sudo rm /usr/local/bin/compare_kdbx
    echo "Le fichier compare_kdbx a été supprimé de /usr/local/bin"
else
    echo "Le fichier compare_kdbx n'existe pas dans /usr/local/bin"
fi

# Vérifie si /usr/local/bin est dans le PATH
if [[ ":$PATH:" == *":/usr/local/bin:"* ]]; then
    # Retire /usr/local/bin du PATH
    export PATH=$(echo $PATH | sed -e 's;:/usr/local/bin;;' -e 's;/usr/local/bin:;;' -e 's;/usr/local/bin;;')
    
    # Supprime /usr/local/bin de .bashrc et .profile
    sed -i '/\/usr\/local\/bin/d' ~/.bashrc
    sed -i '/\/usr\/local\/bin/d' ~/.profile

    echo "/usr/local/bin a été retiré du PATH"
else
    echo "/usr/local/bin n'était pas dans le PATH"
fi

echo "Désinstallation terminée"
