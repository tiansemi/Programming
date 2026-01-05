# 📝 CHANGELOG - Projet Gestion de Bibliothèque

Tous les changements notables de ce projet seront documentés dans ce fichier.

## [Version 1.0.0] - 5 Janvier 2026

### ✨ Nouvelles Fonctionnalités

#### Frontend - Emprunts et Favoris
- **Emprunter des livres** avec sélection de date (date picker)
  - Les utilisateurs peuvent choisir une date de retour (minimum demain)
  - Validation de la disponibilité des copies
  - Intégration API complète avec gestion d'erreurs

- **Système de favoris** avec icônes cœur interactives
  - Toggle ❤️ (favori) / 🤍 (non-favori)
  - Persévérance des favoris en base de données
  - Appels API avec gestion d'erreurs

- **Historique des emprunts amélioré**
  - 3 sections colorées :
    - 🔴 **Emprunts en retard** (red) - Livres dépassant la date de retour
    - 🔵 **Emprunts actifs** (blue) - Livres actuellement empruntés avec jours restants
    - 🟢 **Livres retournés** (green) - Historique des retours
  - Calcul automatique des jours restants
  - Affichage des dates (prévue vs réelle)

- **Retour de livres**
  - Bouton "Return" sur les emprunts actifs
  - Confirmation avant retour
  - Gestion du statut et mise à jour des copies disponibles

#### Admin Panel Amélioré
- **BookForm.jsx** pour création/modification de livres
  - Formulaire complet avec validation
  - Support du mode créer et éditer
  - Gestion des erreurs

- **Affichage de tous les emprunts du système**
  - Tableau avec filtrage possible
  - Informations détaillées sur chaque emprunt
  - Vue d'admin exclusive

#### Composants UI Améliorés
- **BookList.jsx** refactorisé
  - Layout en cards avec meilleure présentation
  - Affichage des informations de livre (titre, auteur, copies)
  - Intégration du date picker pour emprunts
  - Favoris système avec icônes

- **MyBorrowsPage.jsx** restructuré
  - Séparation en 3 tableaux distincts
  - Couleurs visuelles pour status
  - Calculs de dates automatiques

### 🔧 Améliorations Techniques

- **Logging System** intégré
  - `logger.jsx` avec 5 niveaux de log
  - Logs colorés dans la console (DEBUG, INFO, SUCCESS, WARN, ERROR)
  - Traceurs dans AuthContext, App, Login, BookList, MyBorrowsPage
  - `DEBUG_GUIDE.md` pour faciliter le débogage

- **Gestion des Erreurs**
  - Flexible array handling dans les réponses API
  - Try-catch robustes avec user feedback
  - Messages d'erreur clairs et informatifs

- **Optimisations Performance**
  - Migration vers Vite (plus rapide que Create React App)
  - Vite config optimisée pour 0.0.0.0 (accès réseau)
  - Import.meta.env pour environnement

### 📚 Documentation

- **STATUS.md** mis à jour
  - État complet du projet (100% fonctionnel)
  - Checklist détaillée pour validation
  - Statuts des features

- **SUMMARY.md** mis à jour
  - Nombre de fichiers augmenté à 50+
  - Ajout des nouvelles fonctionnalités dans la liste
  - Statistiques du projet

- **CHANGELOG.md** créé
  - Documentation des changements par version
  - Historique complet des mises à jour

---

## Version 0.9.0 - 4 Janvier 2026

### ✨ Fonctionnalités Initiales Complétées

#### Backend (100% Complet)
- Modèles Mongoose (User, Book, Borrow)
- Authentification JWT avec bcryptjs
- 15+ endpoints API
- Middlewares (auth, error handling)
- CRUD complet pour livres
- Système d'emprunts fonctionnel
- Admin panel routes
- CreateAdmin.js pour création d'admin

#### Frontend (90% Complet)
- Authentification (login/register/logout)
- Navigation basée sur les rôles
- Affichage des livres avec recherche
- Filtrage par catégorie
- Pagination des résultats
- AuthContext avec persistence
- Admin panel interface

### 🐛 Corrections de Bugs

- **Double Password Hashing** - Corrigé dans createAdmin.js
- **Borrows.map() is not a function** - Ajout array validation dans AdminPanel
- **GitLab Language Server** - Diagnostic et vérification, fonctionnement normal
- **Vite Port Configuration** - Port 3000 configuré correctement
- **Token Persistence** - localStorage sync implémenté

### 🔒 Sécurité

- Vulnérabilités semver résolues (npm audit fix --force)
- Nodemon mis à jour vers v3.1.11
- JWT secrets correctement configurés
- CORS configuré correctement
- Mots de passe hashés avec bcryptjs

---

## Version 0.5.0 - 2 Janvier 2026

### ✨ Migration Vite

- Migration de Create React App vers Vite
- Création de `index.html` pour Vite
- Création de `main.jsx` comme entry point
- Renamed all .js files with JSX to .jsx
- Updated vite.config.js avec proper configuration
- Fixed import.meta.env pour environment variables

### 🐛 Corrections

- Fixed "process is not defined" error
- Process.env → import.meta.env
- Environment variables avec prefix VITE_

---

## Version 0.2.0 - 29 Novembre 2025

### ✨ Initial Setup

- Création de la structure du projet
- Setup backend (Node.js/Express)
- Setup frontend (React.js)
- Configuration MongoDB
- Documentation initiale (README, INSTALLATION)

---

## 📋 Format des Versions

Nous suivons [Semantic Versioning](https://semver.org/):
- **MAJOR** : Changements incompatibles
- **MINOR** : Nouvelles fonctionnalités compatibles
- **PATCH** : Corrections de bugs compatibles

---

## 🚀 Prochaines Versions

### Version 1.1.0 (Futur)
- [ ] UI/UX améliorée avec Tailwind CSS
- [ ] Notifications en temps réel
- [ ] Upload d'images pour couvertures
- [ ] Système de réservation de livres
- [ ] Recommandations basées sur l'historique

### Version 2.0.0 (Avenir)
- [ ] Application mobile (React Native)
- [ ] Intégration Google Books API
- [ ] Clubs de lecture virtuels
- [ ] Partage social des livres
- [ ] Dashboard statistiques pour admins

---

## 📞 Support et Questions

Pour toute question sur les changements :
1. Consultez le PRESENTATION.md pour la démo
2. Consultez le DEBUG_GUIDE.md pour le débogage
3. Consultez le INSTALLATION.md pour la configuration

---

**Dernière mise à jour** : 5 Janvier 2026
