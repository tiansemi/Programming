#!/usr/bin/env python3

import subprocess
import datetime

# Définir la date et l'heure cibles (Année, Mois, Jour, Heure, Minute)
target_date = datetime.datetime(2023, 11, 10, 13, 30)

#Vérifier si la date et l'heure actuelles correspondent à la date et l'heure cibles
while 1:
    # Obtenir la date et l'heure actuelles
    current_date = datetime.datetime.now().replace(microsecond=0)
    if current_date == target_date:
        # Appeler le lecteur audio externe ['nomProgramme','arguments', 'valeurArguments','nomFichier']
        #arguments et valeurArguments sont optionnel, à mettre selon vos besoins
      #par exemple : subprocess.call(['parole','-r','--name','Alarm TS','./alarm-song.oga']) pour augmenter le volume
      #et définir le nom de la fenêtre à Alarm TS
        subprocess.call(['parole','./alarm-song.oga'])
        break
