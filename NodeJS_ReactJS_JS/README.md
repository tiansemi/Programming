# 📚 Mini-Système de Gestion de Bibliothèque

Un système complet de gestion de bibliothèque construit avec **Node.js/Express** (Backend) et **React.js** (Frontend), connecté à **MongoDB**.

## 🎯 Fonctionnalités

- ✅ **Gestion des livres** (CRUD) - Créer, lire, modifier, supprimer
- ✅ **Gestion des emprunts** - Emprunter et retourner des livres
- ✅ **Mise en favoris** - Ajouter des livres aux favoris
- ✅ **Authentification utilisateur** - Inscription et connexion avec JWT
- ✅ **Rôles utilisateurs** - Admin et Utilisateur
- ✅ **Recherche de livres** - Recherche par titre, auteur, catégorie
- ✅ **Pagination** - Affichage paginé des résultats

## 📁 Structure du Projet

```
Projet Nodejs-Reactjs/
├── backend/                    # API REST Node.js/Express
│   ├── config/
│   │   └── db.js              # Configuration MongoDB
│   ├── models/
│   │   ├── User.js            # Modèle utilisateur
│   │   ├── Book.js            # Modèle livre
│   │   └── Borrow.js          # Modèle emprunt
│   ├── controllers/
│   │   ├── authController.js  # Contrôleur authentification
│   │   ├── bookController.js  # Contrôleur livres
│   │   └── borrowController.js# Contrôleur emprunts
│   ├── routes/
│   │   ├── authRoutes.js      # Routes authentification
│   │   ├── bookRoutes.js      # Routes livres
│   │   └── borrowRoutes.js    # Routes emprunts
│   ├── middleware/
│   │   ├── auth.js            # Middleware d'authentification
│   │   └── errorHandler.js    # Gestionnaire d'erreurs
│   ├── server.js              # Serveur principal
│   ├── package.json           # Dépendances Node.js
│   └── .env.example           # Exemple variables d'environnement
│
└── frontend/                   # Application React.js
    ├── public/
    │   └── index.html         # HTML principal
    ├── src/
    │   ├── components/
    │   │   ├── Login.js       # Composant login
    │   │   ├── Register.js    # Composant inscription
    │   │   ├── BookList.js    # Liste des livres
    │   │   └── Header.js      # En-tête
    │   ├── pages/
    │   │   ├── HomePage.js    # Page d'accueil
    │   │   ├── DashboardPage.js# Page tableau de bord
    │   │   └── MyBorrowsPage.js# Page mes emprunts
    │   ├── services/
    │   │   └── api.js         # Services API
    │   ├── context/
    │   │   └── AuthContext.js # Contexte authentification
    │   ├── App.js             # Composant App
    │   └── index.js           # Point d'entrée
    ├── package.json           # Dépendances React
    └── .env.example           # Variables d'environnement
```

## 🚀 Installation et Démarrage

### Backend

1. **Naviguer au répertoire backend** :
   ```bash
   cd backend
   ```

2. **Créer le fichier .env** à partir du fichier .env.example:
   ```bash
   cp .env.example .env
   ```

3. **Installer les dépendances** :
   ```bash
   npm install
   ```

4. **Démarrer MongoDB** (assurez-vous que MongoDB est en cours d'exécution):
   ```bash
   # Windows: utiliser MongoDB Compass ou le service Windows
   # Linux/Mac: mongod
   ```

5. **Lancer le serveur** :
   ```bash
   npm start          # Production
   npm run dev        # Développement avec nodemon
   ```

Le serveur sera disponible à `http://localhost:5000`

### Frontend

1. **Ouvrir un nouveau terminal et naviguer au répertoire frontend** :
   ```bash
   cd frontend
   ```

2. **Créer le fichier .env** à partir du fichier .env.example:
   ```bash
   cp .env.example .env
   ```

3. **Installer les dépendances** :
   ```bash
   npm install
   ```

4. **Lancer l'application React** :
   ```bash
   npm start
   ```

L'application s'ouvrira automatiquement à `http://localhost:3000`

## 📝 Architecture API

### Routes d'Authentification (`/api/auth`)

| Méthode | Endpoint | Description | Auth |
|---------|----------|-------------|------|
| POST | `/register` | Créer un nouvel utilisateur | ❌ |
| POST | `/login` | Connecter un utilisateur | ❌ |
| GET | `/profile` | Récupérer le profil utilisateur | ✅ |
| POST | `/favorites` | Ajouter un livre aux favoris | ✅ |
| DELETE | `/favorites/:bookId` | Retirer un livre des favoris | ✅ |

### Routes des Livres (`/api/books`)

| Méthode | Endpoint | Description | Auth |
|---------|----------|-------------|------|
| GET | `/` | Récupérer tous les livres (paginé) | ❌ |
| GET | `/search` | Chercher des livres | ❌ |
| GET | `/:id` | Récupérer un livre par ID | ❌ |
| POST | `/` | Créer un nouveau livre | ✅ Admin |
| PUT | `/:id` | Modifier un livre | ✅ Admin |
| DELETE | `/:id` | Supprimer un livre | ✅ Admin |

### Routes des Emprunts (`/api/borrows`)

| Méthode | Endpoint | Description | Auth |
|---------|----------|-------------|------|
| POST | `/` | Emprunter un livre | ✅ |
| PUT | `/:borrowId/return` | Retourner un livre emprunté | ✅ |
| GET | `/user/my-borrows` | Récupérer mes emprunts | ✅ |
| GET | `/` | Récupérer tous les emprunts | ✅ Admin |

## 🔐 Authentification

- Utilise **JWT (JSON Web Tokens)**
- Les tokens sont stockés dans le localStorage du navigateur
- Les tokens expirent après 7 jours
- Les mots de passe sont hachés avec **bcryptjs**

## 📊 Modèles de Données

### User
```javascript
{
  username: String (unique),
  email: String (unique),
  password: String (hashed),
  role: 'user' | 'admin',
  favorites: [Book._id],
  createdAt: Date,
  updatedAt: Date
}
```

### Book
```javascript
{
  title: String,
  author: String,
  isbn: String (unique),
  description: String,
  category: String,
  publishedYear: Number,
  totalCopies: Number,
  availableCopies: Number,
  coverImage: String (URL),
  rating: Number (0-5),
  createdBy: User._id,
  createdAt: Date,
  updatedAt: Date
}
```

### Borrow
```javascript
{
  book: Book._id,
  user: User._id,
  borrowDate: Date,
  returnDate: Date,
  actualReturnDate: Date,
  status: 'borrowed' | 'returned' | 'overdue',
  notes: String,
  createdAt: Date,
  updatedAt: Date
}
```

## 🎨 Technologies Utilisées

### Backend
- **Node.js** - Runtime JavaScript
- **Express.js** - Framework web
- **MongoDB** - Base de données NoSQL
- **Mongoose** - ODM MongoDB
- **JWT** - Authentification sécurisée
- **bcryptjs** - Hachage des mots de passe

### Frontend
- **React.js** - Bibliothèque UI
- **axios** - Client HTTP
- **React Context API** - Gestion d'état globale
- **CSS-in-JS** - Styles inline

## 🔧 Variables d'Environnement

### Backend (.env)
```env
MONGODB_URI=mongodb://localhost:27017/library_db
PORT=5000
NODE_ENV=development
JWT_SECRET=your_super_secret_jwt_key_here
CORS_ORIGIN=http://localhost:3000
```

### Frontend (.env)
```env
REACT_APP_API_URL=http://localhost:5000/api
```

## 📖 Utilisation de l'Application

### Pour un utilisateur normal :
1. S'inscrire ou se connecter
2. Parcourir les livres disponibles
3. Utiliser la recherche et les filtres par catégorie
4. Emprunter des livres (si disponibles)
5. Voir l'historique de ses emprunts
6. Retourner les livres empruntés
7. Ajouter des livres aux favoris

### Pour un administrateur :
1. Accès à un panel d'administration
2. Gérer les livres (créer, modifier, supprimer)
3. Voir tous les emprunts du système
4. Gérer les copies disponibles

## 🐛 Dépannage

### Le backend ne se connecte pas à MongoDB
- Assurez-vous que MongoDB est en cours d'exécution
- Vérifiez la variable `MONGODB_URI` dans le fichier `.env`

### Le frontend ne peut pas se connecter au backend
- Vérifiez que le backend s'exécute sur `http://localhost:5000`
- Vérifiez la variable `REACT_APP_API_URL` dans le `.env` du frontend

### Erreurs d'authentification
- Vérifiez que le `JWT_SECRET` dans le backend est correct
- Assurez-vous que les tokens sont stockés correctement dans le localStorage

## 📝 Notes de Développement

- Le projet utilise le port **5000** pour le backend et **3000** pour le frontend
- Les token JWT expirent après 7 jours
- Les favoris sont stockés au niveau de l'utilisateur
- Les emprunts peuvent être marqués comme "overdue" (en retard)

## 👥 Contribution

Ce projet a été créé pour fins pédagogiques. N'hésitez pas à l'améliorer en ajoutant de nouvelles fonctionnalités !

## 📅 Durée du Projet

- **Début** : 29 novembre 2025
- **Durée** : 1 mois
- **Nombre de groupes** : 2
- **Présentation** : 15-20 minutes par groupe

---

Bon codage ! 🚀📚
