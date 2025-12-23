const express = require('express');
const { createBook, getAllBooks, getBookById, updateBook, deleteBook, searchBooks } = require('../controllers/bookController');
const { authMiddleware, adminMiddleware } = require('../middleware/auth');

const router = express.Router();

// Public routes
router.get('/', getAllBooks);
router.get('/search', searchBooks);
router.get('/:id', getBookById);

// Protected routes (Admin only)
router.post('/', adminMiddleware, createBook);
router.put('/:id', adminMiddleware, updateBook);
router.delete('/:id', adminMiddleware, deleteBook);

module.exports = router;
