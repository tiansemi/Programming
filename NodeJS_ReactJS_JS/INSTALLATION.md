# 🚀 Guide Complet d'Installation et de Configuration

## Prérequis

Avant de commencer, assurez-vous d'avoir installé :

1. **Node.js** (v14 ou supérieur) - [Télécharger](https://nodejs.org/)
2. **MongoDB** (v4.4 ou supérieur) - [Télécharger](https://www.mongodb.com/try/download/community)
3. **Visual Studio Code** (optionnel) - [Télécharger](https://code.visualstudio.com/)

## Étape 1 : Vérifier l'installation de Node.js et MongoDB

### Windows PowerShell

```powershell
# Vérifier Node.js
node --version
npm --version

# Vérifier MongoDB
mongod --version
```

## Étape 2 : Configuration du Backend

### 2.1 Configurer les variables d'environnement

Naviguez vers le dossier backend et créez un fichier `.env` :

```bash
cd backend
```

Créez un fichier `.env` avec le contenu suivant :

```env
# MongoDB
MONGODB_URI=mongodb://localhost:27017/library_db

# Server
PORT=5000
NODE_ENV=development

# JWT
JWT_SECRET=your_super_secret_jwt_key_change_in_production

# CORS
CORS_ORIGIN=http://localhost:3000
```

### 2.2 Installer les dépendances du backend

```bash
npm install
```

**Note sur les vulnérabilités :** Si vous voyez des avertissements sur des vulnérabilités liées à `semver`, exécutez :

```bash
npm audit fix --force
```

Cela installera les packages suivants :
- `express` - Framework web
- `mongoose` - ODM MongoDB
- `dotenv` - Gestion des variables d'environnement
- `cors` - Gestion des CORS
- `bcryptjs` - Hachage des mots de passe
- `jsonwebtoken` - Authentification JWT
- `express-validator` - Validation des entrées
- `nodemon` - (Dev) Rechargement automatique

### 2.3 Démarrer le backend

```bash
# Mode développement (avec rechargement automatique)
npm run dev

# OU mode production
npm start
```

Vous devriez voir :
```
Server running on http://localhost:5000
MongoDB Connected: localhost
```

## Étape 3 : Configuration du Frontend

### 3.1 Configurer les variables d'environnement

Dans un **nouveau terminal**, naviguez vers le dossier frontend :

```bash
cd frontend
```

Créez un fichier `.env` :

```env
REACT_APP_API_URL=http://localhost:5000/api
```

### 3.2 Installer les dépendances du frontend

```bash
npm install
```

Cela installera les packages suivants :
- `react` - Bibliothèque UI
- `react-dom` - DOM React
- `react-router-dom` - Routage
- `axios` - Client HTTP
- `react-scripts` - Outils de build

### 3.3 Lancer l'application React

```bash
npm start
```

L'application s'ouvrira automatiquement à `http://localhost:3000`

## Étape 4 : Configuration de MongoDB

### Option A : Utiliser MongoDB localement (Windows)

1. Installez MongoDB Community Edition
2. MongoDB s'exécutera automatiquement en tant que service Windows
3. Vérifiez la connexion en ouvrant MongoDB Compass et en vous connectant à `mongodb://localhost:27017`

### Option B : Utiliser MongoDB Atlas (Cloud)

1. Créez un compte sur [MongoDB Atlas](https://www.mongodb.com/cloud/atlas)
2. Créez un cluster gratuit
3. Récupérez votre chaîne de connexion
4. Remplacez `MONGODB_URI` dans le `.env` du backend par votre chaîne de connexion :

```env
MONGODB_URI=mongodb+srv://username:password@cluster0.xxxxx.mongodb.net/library_db?retryWrites=true&w=majority
```

## Étape 5 : Test de l'Application

### 5.1 Créer un compte utilisateur

1. Allez sur `http://localhost:3000`
2. Cliquez sur "Register"
3. Remplissez le formulaire :
   - Username : `testuser`
   - Email : `test@example.com`
   - Password : `password123`
4. Cliquez sur "Register"

### 5.2 Se connecter

1. Cliquez sur "Login"
2. Entrez vos identifiants
3. Cliquez sur "Login"

### 5.3 Tester les fonctionnalités

- Parcourez les livres
- Utilisez la recherche
- Filtrez par catégorie

## Étape 6 : Créer des données test (Admin)

Pour avoir des livres dans la base de données, vous devez créer un compte admin.

### Via MongoDB Compass

1. Ouvrez MongoDB Compass
2. Connectez-vous à `mongodb://localhost:27017`
3. Accédez à la base de données `library_db`
4. Dans la collection `users`, trouvez votre utilisateur
5. Modifiez le champ `role` de `user` à `admin`

### Via ligne de commande MongoDB

```bash
# Accédez à MongoDB shell
mongosh

# Sélectionnez la base de données
use library_db

# Mettez à jour votre utilisateur en admin
db.users.updateOne(
  { email: "test@example.com" },
  { $set: { role: "admin" } }
)
```

## Dépannage Courant

### ❌ "MongoDB connection failed"

**Solution :**
- Vérifiez que MongoDB est démarré
- Vérifiez la variable `MONGODB_URI` dans `.env`
- Assurez-vous que le port MongoDB (27017) n'est pas bloqué

### ❌ "Cannot find module 'express'"

**Solution :**
```bash
cd backend
npm install
```

### ❌ "Port 5000 already in use"

**Solution :**
- Changez le PORT dans le `.env` (ex: 5001)
- OU tuez le processus qui utilise le port 5000

### ❌ "CORS error"

**Solution :**
- Vérifiez que `CORS_ORIGIN` dans le backend correspond à l'URL du frontend
- Par défaut : `http://localhost:3000`

### ❌ "Token expired"

**Solution :**
- Se reconnecter pour obtenir un nouveau token
- Les tokens expirent après 7 jours

## Procédures Courantes

### Vider la base de données

```bash
# Via MongoDB Compass
# Clic droit sur la base de données -> Drop Database

# Via MongoDB Shell
mongosh
use library_db
db.dropDatabase()
```

### Réinitialiser les dépendances

```bash
# Backend
cd backend
rm -r node_modules
npm install

# Frontend
cd frontend
rm -r node_modules
npm install
```

### Tuer les processus

```powershell
# Windows PowerShell
# Trouver le processus
Get-Process -Name node

# Tuer le processus
Stop-Process -Name node -Force
```

## Structure des dossiers attendue

```
Projet Nodejs-Reactjs/
├── README.md
├── INSTALLATION.md (ce fichier)
├── backend/
│   ├── node_modules/
│   ├── config/
│   ├── controllers/
│   ├── middleware/
│   ├── models/
│   ├── routes/
│   ├── .env
│   ├── .env.example
│   ├── .gitignore
│   ├── package.json
│   ├── package-lock.json
│   └── server.js
└── frontend/
    ├── node_modules/
    ├── public/
    ├── src/
    ├── .env
    ├── .env.example
    ├── .gitignore
    ├── package.json
    ├── package-lock.json
    └── README.md
```

## Points de vérification

✅ Node.js et npm installés
✅ MongoDB installé et démarré
✅ Backend : `.env` configuré avec les bonnes variables
✅ Backend : `npm install` exécuté
✅ Backend : `npm run dev` exécuté avec succès
✅ Frontend : `.env` configuré
✅ Frontend : `npm install` exécuté
✅ Frontend : `npm start` exécuté avec succès
✅ Navigation vers `http://localhost:3000` fonctionne
✅ Création de compte et connexion fonctionnent
✅ Les livres s'affichent (si des données existent)

## 📞 Support

Si vous rencontrez d'autres problèmes :
1. Vérifiez les logs du serveur backend
2. Utilisez les outils de développement du navigateur (F12)
3. Vérifiez la console MongoDB pour les erreurs

---

Vous êtes maintenant prêt à développer ! 🚀
