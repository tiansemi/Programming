const express = require('express');
const { registerUser, loginUser, getUserProfile, addToFavorites, removeFromFavorites } = require('../controllers/authController');
const { authMiddleware } = require('../middleware/auth');

const router = express.Router();

// Public routes
router.post('/register', registerUser);
router.post('/login', loginUser);

// Protected routes
router.get('/profile', authMiddleware, getUserProfile);
router.post('/favorites', authMiddleware, addToFavorites);
router.delete('/favorites/:bookId', authMiddleware, removeFromFavorites);

module.exports = router;
