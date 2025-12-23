const express = require('express');
const { borrowBook, returnBook, getUserBorrows, getAllBorrows } = require('../controllers/borrowController');
const { authMiddleware, adminMiddleware } = require('../middleware/auth');

const router = express.Router();

// Protected routes
router.post('/', authMiddleware, borrowBook);
router.put('/:borrowId/return', authMiddleware, returnBook);
router.get('/user/my-borrows', authMiddleware, getUserBorrows);

// Admin routes
router.get('/', adminMiddleware, getAllBorrows);

module.exports = router;
