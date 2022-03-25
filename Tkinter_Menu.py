#!/usr/bin/python3
#coding=Latin1

"""
	@Author : Moulo Oholo

	@Describe : Etudiant en Licence 3 à L'Université Polytechnique de Bingerville - Côte d'Ivoire

	@Date : Jeudi, 24 Mars 2022
	
	@License : GPL
	
	@Entreprise : TianSemi > Tiansemi Programming
	
	@This code : Ce programme cré une application GUI avec Tkinter. Celle-ci ne contient q'une barre de menu moderne que l'on voit généralement sur la majorité des logiciels avec des Onglets et Options de commandes à l'intérieur.
	
	@Contact : +225 01 42 09 38 91  || tiansemi@outlook.com
	
	@Using : Lancer le script avec l'interprèteur Python : 
				$ python Tkinter_Menu.py
			Dans certains cas, selon la manière de l'installation de Python, il faudra specifier la version:
				$ python3 Tkinter_Menu.py
			Bien évidemment vous devez être dans le repertoire où se trouve le script, sinon tapez tout le chemin d'accès au fichier(le chemin absolu)

	@github : https://github.com/tiansemi/

	@Linkedin : https://linkedin.com/in/jeannoelmoulo/

"""


import tkinter as tk

root = tk.Tk()# Crée La fenêtre principale
root.title("TianSemi")# Définie le titre de l'application
root.resizable(0,0)# Désactive le redimensionnement
root.geometry('260x200')# Définie la taille de l'application
print(root.tk.call('tk', 'windowingsystem'))# Pour connaitre l'OS sur lequel le programme est exécuté
root.option_add('*tearOff',False)# Evite le détachement des sous menus
menu_bar = tk.Menu(root,name='menubar')# Crée la barre de menu

tabFile = tk.Menu(menu_bar)# Crée de l'onglet 'File' qui est un sous menu de la barre de menu
menu_bar.add_cascade(menu=tabFile,label='File',underline=0)# Fixer le sous menu à la barre de menu

tabFile.add_command(label='New File',accelerator='Ctrl-N')#Ajoute l'option 'New File'

icofile = tk.PhotoImage(file='icofile.png')# Charge l'image 'icofile.png' dans la variable icofile
tabFile.add_command(label='Open File',accelerator='Ctrl-O',underline=0,image=icofile,compound='left')#Ajoute l'option 'Open File' avec les attributs souhaités

tabFile.add_command(label='Close File',accelerator='Ctrl-W')#Ajoute l'option 'Close File' avec l'attribut souhaité

tabFile.add_command(label='Open Folder', accelerator='Ctrl-O',  state="disabled")# Désactive l'option 'Open Folder' avec les attributs souhaités

tabFile.insert(index=2,itemType='command',label='Save File')#Ajoute l'option 'Save File' après la deuxième option en comptant à partir de 0

tabFile.delete(0)#Supprime la première option

tabFile.add_separator()#Ajoute un séparateur

#Création du menu déroulant
SaveAs = tk.Menu(tabFile)# Crée de l'onglet 'Save As...'
tabFile.add_cascade(menu=SaveAs,label='Save As...',underline=0,accelerator='Ctrl-S')#Ajoute le menu déroulant à l'onglet 'File'
# Pour une seule sélection possible dans la liste
radio = str()# La variable qui contiendra la valeur de l'élément sélectionner
SaveAs.add_radiobutton(label='Python File',variable=radio,value=1)# premier élément de la liste
SaveAs.add_radiobutton(label='Plain Text',variable=radio,value=2)# Deuxième élément de la liste
SaveAs.add_radiobutton(label='PDF File',variable=radio,value=3)# Troisieme élément de la liste
# Pour une multiple sélections
check = str()
tabFile.add_checkbutton(label='Show line number', variable=check, onvalue=1, offvalue=0)# Premier élément de la liste
tabFile.add_checkbutton(label='Show map', variable=check, onvalue=1, offvalue=0)# Deuxième élément de la liste

print( tabFile.entrycget(0, 'label'))# Affiche le label du premier élément de l'onglet 'File'
tabFile.entryconfigure('Close File', state='disabled')# Modifie l'état de l'élément 'Close File'
# print( tabFile.entryconfigure(0) )

# menu système - Faire des recherches
sysmenu = tk.Menu(menu_bar,name='system')
menu_bar.add_cascade(menu=sysmenu,label="Moulo")
sysmenu.add_command(label="test")

menuContextual = tk.Menu(root)
for i in ('Un', 'Deux', 'Trois'):
	menuContextual.add_command(label=i)
	if (root.tk.call('tk', 'windowingsystem')=='aqua'):# Dans le cas où nous sommes sur l'OS MacOSX alors
		root.bind('<2>', lambda e:	menuContextual.post(e.x_root, e.y_root))# Affiche le menu contextuel à l'emplacement du click grace à la methode post() lors d'un click droit
		root.bind('<Control-1>', lambda e:	menuContextual.post(e.x_root, e.y_root))# Affiche le menu contextuel à l'emplacement du click grace à la methode post() lors d'un click gauche combiné avec 'control'
	else:
		root.bind('<3>', lambda e: menuContextual.post(e.x_root, e.y_root))# Affiche le menu contextuel à l'emplacement du click grace à la methode post() lors d'un click droit


root.config(menu=menu_bar)# Spécifie la barre de menus de la fenêtre
root.mainloop()# La boucle pour maintenir active l'application
