import axios from 'axios';

// Utilisez import.meta.env au lieu de process.env avec Vite
const API_URL = import.meta.env.VITE_API_URL || 'http://localhost:5000/api';

const api = axios.create({
  baseURL: API_URL,
});

// Intercepteur pour ajouter le token JWT
api.interceptors.request.use((config) => {
  const token = localStorage.getItem('token');
  if (token) {
    config.headers.Authorization = `Bearer ${token}`;
  }
  return config;
});

// Auth Service
export const authService = {
  register: (username, email, password) =>
    api.post(`/auth/register`, { username, email, password }),
  
  login: (email, password) =>
    api.post(`/auth/login`, { email, password }),
  
  getProfile: () =>
    api.get(`/auth/profile`),
};

// Book Service
export const bookService = {
  getAllBooks: (search = '', category = '', page = 1, limit = 10) =>
    api.get(`/books`, {
      params: { search, category, page, limit },
    }),
  
  getBookById: (id) =>
    api.get(`/books/${id}`),
  
  searchBooks: (query, category = '') =>
    api.get(`/books/search`, {
      params: { q: query, category },
    }),
  
  createBook: (bookData) =>
    api.post(`/books`, bookData),
  
  updateBook: (id, bookData) =>
    api.put(`/books/${id}`, bookData),
  
  deleteBook: (id) =>
    api.delete(`/books/${id}`),
};

// Borrow Service
export const borrowService = {
  borrowBook: (bookId, returnDate) =>
    api.post(
      `/borrows`,
      { bookId, returnDate }
    ),
  
  returnBook: (borrowId) =>
    api.put(`/borrows/${borrowId}/return`, {}),
  
  getUserBorrows: () =>
    api.get(`/borrows/user/my-borrows`),
  
  getAllBorrows: (status = '', page = 1, limit = 10) =>
    api.get(`/borrows`, {
      params: { status, page, limit },
    }),
};

// Favorites Service
export const favoriteService = {
  addToFavorites: (bookId) =>
    api.post(
      `/auth/favorites`,
      { bookId }
    ),
  
  removeFromFavorites: (bookId) =>
    api.delete(`/auth/favorites/${bookId}`),
};
