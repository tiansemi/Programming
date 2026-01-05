# 🔍 Guide de Débogage - Flux de Connexion

## Comment utiliser les logs de débogage

### Ouvrir la Console du Navigateur
1. **F12** ou **Ctrl+Shift+I** (Windows/Linux)
2. Allez dans l'onglet **Console**
3. Vous verrez les logs colorés du système

### Flux de Logs à Observer

Quand vous **testez une connexion**, vous devriez voir cet ordre de logs :

```
🚀 Application started
[AuthProvider mounted, checking localStorage for token]
[App RENDERED] - isAuthenticated: false
[Login - LOGIN FORM SUBMITTED] - email: user@example.com
[Login - Sending login request to API]
[Login - API login response received] - user: testuser, tokenLength: ...
[Login - Calling context login()]
[AuthContext - === LOGIN CALLED ===]
[AuthContext - Login successful]
[App RENDERED] - isAuthenticated: true, user: testuser
✅ User authenticated, showing DashboardPage
```

### Les Niveaux de Logs

- 🟦 **DEBUG** (Bleu) - Informations détaillées du flow
- 🟦 **INFO** (Bleu clair) - Étapes importantes
- 🟩 **✅ SUCCESS** (Vert) - Actions réussies
- 🟨 **⚠️ WARN** (Orange) - Avertissements
- 🟥 **❌ ERROR** (Rouge) - Erreurs

### Que Observer

#### ✅ Si le login fonctionne correctement :
- Les logs montrent login -> context updated -> App re-rendered
- Le componant DashboardPage s'affiche
- Pas de redirection vers HomePage

#### ❌ Si vous êtes redirigé après login :
- Les logs montreront un break dans la chaîne
- Par exemple : "Login successful" mais pas "App RENDERED - isAuthenticated: true"
- Cela veut dire que le state du context ne persiste pas

### Où chercher les bugs

**Cas 1 : "Token not saved in localStorage"**
- Vérifiez dans AuthContext qu'`useEffect` sauvegarde le token
- Ouvrez DevTools -> Application -> Local Storage -> Vérifiez la présence de `token` et `user`

**Cas 2 : "Login succeeds but App doesn't recognize auth"**
- Vérifiez que `App.jsx` lit bien `token` du context (pas juste `user`)
- Vérifiez que les conditions `if (token && user)` sont correctes

**Cas 3 : "AuthProvider not mounting at startup"**
- Vérifiez que `App.jsx` wrappe bien le contenu dans `<AuthProvider>`
- Vous devriez voir "[AuthProvider mounted...]" dans les logs

### Script de Test Rapide (Console du Navigateur)

Collez ceci dans la console pour vérifier le localStorage :

```javascript
console.log('Token:', localStorage.getItem('token'));
console.log('User:', localStorage.getItem('user'));
console.log('All items:', { ...localStorage });
```

### Réinitialiser le State (Debug)

Si vous êtes bloqué, videz le localStorage et rechargez :

```javascript
localStorage.clear();
location.reload();
```

---

## Configuration

### Fichiers modifiés
- `vite.config.js` - Ajout de `host: '0.0.0.0'` pour accès réseau
- `src/utils/logger.jsx` - System de logging centralisé
- `src/context/AuthContext.jsx` - Logs du contexte d'authentification
- `src/App.jsx` - Logs du routing et vérification d'auth
- `src/components/Login.jsx` - Logs du flux de connexion

### Variables d'environnement
```env
VITE_API_URL=http://localhost:5000/api
```

---

## Commandes de Démarrage

```bash
# Backend
cd backend
npm run dev

# Frontend (nouveau terminal)
cd frontend
npm start
```

L'app sera accessible sur :
- Local: http://localhost:3000/
- Réseau: http://0.0.0.0:3000/ (ou votre IP locale)

---

## Checklist de Débogage

- [ ] Backend démarré (port 5000)
- [ ] Frontend démarré (port 3000)
- [ ] Console navigateur ouverte (F12)
- [ ] localStorage visible (DevTools -> Application -> Local Storage)
- [ ] Logs colorés affichés
- [ ] Inscription fonctionne (nouveau compte créé)
- [ ] Login fonctionne (token sauvegardé)
- [ ] DashboardPage s'affiche (pas de redirection)
- [ ] Logout fonctionne (localStorage vidé, HomePage affichée)

---

Pour toute question, vérifiez les logs en priorité ! 🔍
