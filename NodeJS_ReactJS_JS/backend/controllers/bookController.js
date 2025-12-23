const Book = require('../models/Book');

// Create Book (Admin Only)
const createBook = async (req, res) => {
  try {
    const { title, author, isbn, description, category, publishedYear, totalCopies, coverImage } = req.body;

    const book = new Book({
      title,
      author,
      isbn,
      description,
      category,
      publishedYear,
      totalCopies,
      availableCopies: totalCopies,
      coverImage,
      createdBy: req.userId,
    });

    await book.save();
    res.status(201).json({ message: 'Book created successfully', book });
  } catch (error) {
    res.status(500).json({ message: error.message });
  }
};

// Get All Books
const getAllBooks = async (req, res) => {
  try {
    const { search, category, page = 1, limit = 10 } = req.query;
    const skip = (page - 1) * limit;

    let query = {};

    if (search) {
      query.$or = [
        { title: { $regex: search, $options: 'i' } },
        { author: { $regex: search, $options: 'i' } },
        { description: { $regex: search, $options: 'i' } },
      ];
    }

    if (category) {
      query.category = category;
    }

    const books = await Book.find(query)
      .populate('createdBy', 'username email')
      .skip(skip)
      .limit(parseInt(limit));

    const total = await Book.countDocuments(query);

    res.json({
      books,
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

// Get Single Book
const getBookById = async (req, res) => {
  try {
    const book = await Book.findById(req.params.id).populate('createdBy', 'username email');
    if (!book) {
      return res.status(404).json({ message: 'Book not found' });
    }
    res.json(book);
  } catch (error) {
    res.status(500).json({ message: error.message });
  }
};

// Update Book (Admin Only)
const updateBook = async (req, res) => {
  try {
    const { title, author, description, category, publishedYear, totalCopies, coverImage, rating } = req.body;

    let book = await Book.findById(req.params.id);
    if (!book) {
      return res.status(404).json({ message: 'Book not found' });
    }

    // Update fields
    if (title) book.title = title;
    if (author) book.author = author;
    if (description) book.description = description;
    if (category) book.category = category;
    if (publishedYear) book.publishedYear = publishedYear;
    if (coverImage) book.coverImage = coverImage;
    if (rating) book.rating = rating;
    if (totalCopies) {
      const difference = totalCopies - book.totalCopies;
      book.totalCopies = totalCopies;
      book.availableCopies += difference;
    }

    await book.save();
    res.json({ message: 'Book updated successfully', book });
  } catch (error) {
    res.status(500).json({ message: error.message });
  }
};

// Delete Book (Admin Only)
const deleteBook = async (req, res) => {
  try {
    const book = await Book.findByIdAndDelete(req.params.id);
    if (!book) {
      return res.status(404).json({ message: 'Book not found' });
    }
    res.json({ message: 'Book deleted successfully' });
  } catch (error) {
    res.status(500).json({ message: error.message });
  }
};

// Search Books
const searchBooks = async (req, res) => {
  try {
    const { q, category } = req.query;

    let query = {};

    if (q) {
      query.$or = [
        { title: { $regex: q, $options: 'i' } },
        { author: { $regex: q, $options: 'i' } },
      ];
    }

    if (category) {
      query.category = category;
    }

    const books = await Book.find(query).populate('createdBy', 'username email').limit(20);
    res.json(books);
  } catch (error) {
    res.status(500).json({ message: error.message });
  }
};

module.exports = {
  createBook,
  getAllBooks,
  getBookById,
  updateBook,
  deleteBook,
  searchBooks,
};
