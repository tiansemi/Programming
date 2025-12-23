const Borrow = require('../models/Borrow');
const Book = require('../models/Book');

// Borrow a Book
const borrowBook = async (req, res) => {
  try {
    const { bookId, returnDate } = req.body;

    const book = await Book.findById(bookId);
    if (!book) {
      return res.status(404).json({ message: 'Book not found' });
    }

    if (book.availableCopies <= 0) {
      return res.status(400).json({ message: 'No copies available' });
    }

    const borrow = new Borrow({
      book: bookId,
      user: req.userId,
      returnDate,
    });

    await borrow.save();
    book.availableCopies -= 1;
    await book.save();

    res.status(201).json({ message: 'Book borrowed successfully', borrow });
  } catch (error) {
    res.status(500).json({ message: error.message });
  }
};

// Return a Book
const returnBook = async (req, res) => {
  try {
    const { borrowId } = req.params;

    const borrow = await Borrow.findById(borrowId);
    if (!borrow) {
      return res.status(404).json({ message: 'Borrow record not found' });
    }

    if (borrow.status === 'returned') {
      return res.status(400).json({ message: 'Book already returned' });
    }

    borrow.actualReturnDate = new Date();
    borrow.status = 'returned';
    await borrow.save();

    const book = await Book.findById(borrow.book);
    book.availableCopies += 1;
    await book.save();

    res.json({ message: 'Book returned successfully', borrow });
  } catch (error) {
    res.status(500).json({ message: error.message });
  }
};

// Get User's Borrows
const getUserBorrows = async (req, res) => {
  try {
    const borrows = await Borrow.find({ user: req.userId })
      .populate('book')
      .sort({ borrowDate: -1 });

    res.json(borrows);
  } catch (error) {
    res.status(500).json({ message: error.message });
  }
};

// Get All Borrows (Admin)
const getAllBorrows = async (req, res) => {
  try {
    const { status, page = 1, limit = 10 } = req.query;
    const skip = (page - 1) * limit;

    let query = {};
    if (status) {
      query.status = status;
    }

    const borrows = await Borrow.find(query)
      .populate('book')
      .populate('user', 'username email')
      .skip(skip)
      .limit(parseInt(limit))
      .sort({ borrowDate: -1 });

    const total = await Borrow.countDocuments(query);

    res.json({
      borrows,
      pagination: {
        total,
        pages: Math.ceil(total / limit),
        currentPage: parseInt(page),
      },
    });
  } catch (error) {
    res.status(500).json({ message: error.message });
  }
};

module.exports = {
  borrowBook,
  returnBook,
  getUserBorrows,
  getAllBorrows,
};
