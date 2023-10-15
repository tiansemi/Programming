#!/usr/bin/env python3.10

"""
@Author : Moulo Oholo
@Contact : tiansemi@outlook.com

Ce programme permet de télécharger les médias d'un canal Telegram
et de les envoyés sur votre cloud mega du site mega.nz.

Vous aurez besoin d'apporter :
    - L'e-mail de votre compte mega [dans la variable mail]
    
    - Le mot de passe de votre compte [dans la variable password]
    
    - L'id de votre api Telegram [dans la variable api_id]
    
    - Le hash de l'api [dans la variable api_hash]
    
    - Lien ou l'id du canal ou groupe ou compte Telegram où vous 
    	souhaité télécharger les médias (images, videos, documents pdf, 
    	txt, epub, etc) [dans la variable channel_link]
    
    - Le chemin d'un fichier texte vide
    	[dans la variable downloaded_files_path]


Vous devez peut être exécuter la commande `chmod u+x <nom_du_fichier>` 
pour les droits d'exécution.
"""


import time
import os
from mega import Mega 
from telethon.sync import TelegramClient
from telethon.tl.types import InputMessagesFilterPhotos, InputMessagesFilterDocument



mail='my.mail@gmail.com'
password='my_password'

api_id = 12345678
api_hash = 'hash_of_my_api'
channel_link = 'channel_link_or_id'  # Vous pouvez le changer par l'id du canal sans les quotes

# Se connecter à votre compte mega
mega = Mega() 
m = mega.login(mail, password) 

# Crée un fichier pour stocker les noms des documents déjà téléchargés
# Le fichier de downloaded_files_path doit obligatoirement existé
downloaded_files_path = 'already_existing_file_path'

# Obtient le contenu du repertoire courant
content_dir = os.listdir(os.getcwd())

# Ouvre le fichier de downloaded_files_path et ajoute tous les 
# noms de fichiers à l'ensemble downloaded_files
downloaded_files = set()
with open(downloaded_files_path, 'r') as f:
    for line in f:
        downloaded_files.add(line.strip())

print("List of documents already uploaded : ")
for file in downloaded_files:
    print(file)

with TelegramClient('telegram_session01', api_id, api_hash) as client:
    print("\n\n==============================\n Connection to Telegram successful.\n==============================")

    # Pour télécharger tous médias confondus sauf les stickers alors
    # décommenter les deux ligne suivantes (L70-L71) et commenter L73-L75
    #for i, message in enumerate(client.iter_messages(channel_link, reverse=True)):
        #if message.media and not message.sticker:

    for i, message in enumerate(client.iter_messages(channel_link, reverse=False, filter=InputMessagesFilterDocument)):
        print(f"[/-+-\\] Message {i+1}")
        if True:
            # Vérifie si le fichier a déjà été téléchargé
            if message.file.name in downloaded_files:
                print(f"Media {i+1} has already been downloaded. Go to the next one.\n")
                continue

            # Supprime le fichier s'il existe déjà
            if message.file.name in content_dir:
                os.remove(message.file.name)

            # Télécharge le fichier temporairement
            print(f"[+] Downloading message media {i+1}...")
            print("<--- ",message.file.name)
            filename= client.download_media(message)
            print(f"[+] Media {i+1} downloaded successfully.")
            # Envoie le fichier sur MEGA
            print(f"[+] Sending the media {i+1} on MEGA...")
            m.upload(filename)
            print("---> ",filename)
            print(f"[+] Media {i+1} successfully sent to MEGA.")
            # Supprime le fichier téléchargé
            os.remove(filename)
            print(f"[+] Media {i+1} successfully deleted.\n")
            # Ajoute le nom du fichier au fichier de downloaded_files_path
            with open(downloaded_files_path, 'a') as f:
                f.write(filename + '\n')
            # L'ajouter aussi à l'ensemble downloaded_files
            downloaded_files.add(filename)
            # Pour laisser à Telegram le temps de soufler un peu (*⌣*)
            time.sleep(10)
            if (i+1) % 20 == 0:
                time.sleep(80)
