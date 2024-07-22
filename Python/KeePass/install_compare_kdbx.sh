#!/bin/bash

# Télécharge le script compare_kdbx.py
curl -O https://raw.githubusercontent.com/tiansemi/Programming/main/Python/KeePass/compare_kdbx.py

# Vérifie si le téléchargement a réussi
if [ ! -f compare_kdbx.py ]; then
    echo "Erreur: le téléchargement de compare_kdbx.py a échoué."
    exit 1
fi

# Rend le script exécutable
chmod +x compare_kdbx.py

# Vérifie si /usr/local/bin est dans le PATH
if [[ ":$PATH:" == *":/usr/local/bin:"* ]]; then
    echo "/usr/local/bin est déjà dans le PATH"
else
    echo "/usr/local/bin n'est pas dans le PATH, l'ajout"
    export PATH=$PATH:/usr/local/bin
    echo 'export PATH=$PATH:/usr/local/bin' >> ~/.bashrc
    echo 'export PATH=$PATH:/usr/local/bin' >> ~/.profile
fi

# Déplace le script dans /usr/local/bin et le renomme en compare_kdbx
sudo mv compare_kdbx.py /usr/local/bin/compare_kdbx

# Vérifie si le déplacement a réussi
if [ -f /usr/local/bin/compare_kdbx ]; then
    echo "Le script compare_kdbx.py a été déplacé avec succès vers /usr/local/bin et peut être exécuté via 'compare_kdbx'"
else
    echo "Erreur: le déplacement de compare_kdbx.py a échoué."
    exit 1
fi
