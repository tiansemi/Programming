import React, { useState, useEffect } from 'react'
import { bookService, borrowService, favoriteService } from '../services/api'
import { useAuth } from '../context/AuthContext'
import createLogger from '../utils/logger'

const logger = createLogger('BookList')

const BookList = () => {
  const { token } = useAuth()
  const [books, setBooks] = useState([])
  const [search, setSearch] = useState('')
  const [category, setCategory] = useState('')
  const [loading, setLoading] = useState(false)
  const [page, setPage] = useState(1)
  const [favorites, setFavorites] = useState([])
  const [borrowingBook, setBorrowingBook] = useState(null)
  const [returnDate, setReturnDate] = useState('')
  const [error, setError] = useState('')
  const [success, setSuccess] = useState('')

  const fetchBooks = React.useCallback(async () => {
    setLoading(true)
    try {
      logger.debug('Fetching books', { search, category, page })
      const response = await bookService.getAllBooks(search, category, page, 10)
      setBooks(response.data.books)
    } catch (error) {
      logger.error('Error fetching books', error)
      setError('Erreur lors du chargement des livres')
    } finally {
      setLoading(false)
    }
  }, [search, category, page])

  useEffect(() => {
    fetchBooks()
  }, [fetchBooks])

  const handleSearch = (e) => {
    e.preventDefault()
    setPage(1)
  }

  // ✅ Emprunter un livre
  const handleBorrowBook = async (bookId) => {
    if (!returnDate) {
      setError('Veuillez sélectionner une date de retour')
      return
    }

    try {
      logger.info('Borrowing book', { bookId, returnDate })
      await borrowService.borrowBook(bookId, returnDate)
      logger.success('Book borrowed successfully')
      setSuccess('Livre emprunté avec succès')
      setBorrowingBook(null)
      setReturnDate('')
      fetchBooks() // Refresh list
      setTimeout(() => setSuccess(''), 3000)
    } catch (err) {
      logger.error('Failed to borrow book', err)
      setError(err.response?.data?.message || 'Erreur lors de l\'emprunt')
    }
  }

  // ✅ Ajouter/Retirer des favoris
  const handleToggleFavorite = async (bookId) => {
    try {
      if (favorites.includes(bookId)) {
        logger.info('Removing from favorites', { bookId })
        await favoriteService.removeFromFavorites(bookId)
        setFavorites(favorites.filter((id) => id !== bookId))
        setSuccess('Retiré des favoris')
      } else {
        logger.info('Adding to favorites', { bookId })
        await favoriteService.addToFavorites(bookId)
        setFavorites([...favorites, bookId])
        setSuccess('Ajouté aux favoris')
      }
      setTimeout(() => setSuccess(''), 2000)
    } catch (err) {
      logger.error('Failed to toggle favorite', err)
      setError('Erreur lors de la gestion des favoris')
    }
  }

  // Calculer date minimale pour retour (demain)
  const minDate = new Date()
  minDate.setDate(minDate.getDate() + 1)
  const minDateStr = minDate.toISOString().split('T')[0]

  return (
    <div style={{ padding: '20px' }}>
      <h2>📚 Livres Disponibles</h2>

      {error && (
        <div style={{ color: 'red', padding: '10px', marginBottom: '10px', backgroundColor: '#ffe6e6', borderRadius: '4px' }}>
          {error}
        </div>
      )}

      {success && (
        <div style={{ color: 'green', padding: '10px', marginBottom: '10px', backgroundColor: '#e6ffe6', borderRadius: '4px' }}>
          {success}
        </div>
      )}

      <form onSubmit={handleSearch} style={{ marginBottom: '20px' }}>
        <div style={{ display: 'flex', gap: '10px', marginBottom: '10px' }}>
          <input
            type="text"
            placeholder="Chercher par titre ou auteur..."
            value={search}
            onChange={(e) => setSearch(e.target.value)}
            style={{ flex: 1, padding: '8px', borderRadius: '4px', border: '1px solid #ccc' }}
          />
          <select
            value={category}
            onChange={(e) => setCategory(e.target.value)}
            style={{ padding: '8px', borderRadius: '4px', border: '1px solid #ccc' }}
          >
            <option value="">Toutes les Catégories</option>
            <option value="Fiction">Fiction</option>
            <option value="Non-Fiction">Non-Fiction</option>
            <option value="Science">Science</option>
            <option value="Histoire">Histoire</option>
            <option value="Biographie">Biographie</option>
            <option value="Romance">Romance</option>
            <option value="Thriller">Thriller</option>
          </select>
          <button
            type="submit"
            style={{
              padding: '8px 16px',
              backgroundColor: '#007bff',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              cursor: 'pointer',
              fontWeight: 'bold',
            }}
          >
            🔍 Chercher
          </button>
        </div>
      </form>

      {loading ? (
        <p>Chargement des livres...</p>
      ) : books.length === 0 ? (
        <p>Aucun livre trouvé</p>
      ) : (
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(280px, 1fr))', gap: '20px' }}>
          {books.map((book) => (
            <div
              key={book._id}
              style={{
                border: '1px solid #ddd',
                padding: '15px',
                borderRadius: '8px',
                backgroundColor: '#f9f9f9',
                boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
              }}
            >
              {book.coverImage && (
                <img
                  src={book.coverImage}
                  alt={book.title}
                  style={{ width: '100%', height: '200px', objectFit: 'cover', borderRadius: '4px', marginBottom: '10px' }}
                />
              )}

              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'start', marginBottom: '10px' }}>
                <div>
                  <h3 style={{ margin: '0 0 5px 0' }}>{book.title}</h3>
                  <p style={{ margin: '0', color: '#666' }}>{book.author}</p>
                </div>
                {token && (
                  <button
                    onClick={() => handleToggleFavorite(book._id)}
                    style={{
                      backgroundColor: 'transparent',
                      border: 'none',
                      fontSize: '20px',
                      cursor: 'pointer',
                      padding: '5px',
                    }}
                    title={favorites.includes(book._id) ? 'Retirer des favoris' : 'Ajouter aux favoris'}
                  >
                    {favorites.includes(book._id) ? '❤️' : '🤍'}
                  </button>
                )}
              </div>

              <p style={{ margin: '5px 0', fontSize: '14px' }}>
                <strong>Catégorie:</strong> {book.category}
              </p>
              <p style={{ margin: '5px 0', fontSize: '14px' }}>
                <strong>Année:</strong> {book.publishedYear}
              </p>
              <p
                style={{
                  margin: '5px 0',
                  fontSize: '14px',
                  color: book.availableCopies === 0 ? 'red' : 'green',
                  fontWeight: 'bold',
                }}
              >
                <strong>Disponible:</strong> {book.availableCopies}/{book.totalCopies}
              </p>
              <p style={{ margin: '5px 0', fontSize: '14px' }}>
                <strong>Note:</strong> ⭐ {book.rating}/5
              </p>

              {book.description && (
                <p style={{ margin: '10px 0', fontSize: '13px', color: '#555', maxHeight: '60px', overflow: 'hidden' }}>
                  {book.description.substring(0, 100)}...
                </p>
              )}

              {/* Bouton Emprunter */}
              {token && book.availableCopies > 0 && (
                <div style={{ marginTop: '10px' }}>
                  {borrowingBook === book._id ? (
                    <div style={{ backgroundColor: '#f0f0f0', padding: '10px', borderRadius: '4px' }}>
                      <label style={{ display: 'block', marginBottom: '5px', fontWeight: 'bold' }}>
                        Date de retour prévue :
                      </label>
                      <input
                        type="date"
                        value={returnDate}
                        onChange={(e) => setReturnDate(e.target.value)}
                        min={minDateStr}
                        style={{
                          width: '100%',
                          padding: '8px',
                          boxSizing: 'border-box',
                          marginBottom: '10px',
                          borderRadius: '4px',
                          border: '1px solid #ccc',
                        }}
                      />
                      <div style={{ display: 'flex', gap: '5px' }}>
                        <button
                          onClick={() => handleBorrowBook(book._id)}
                          style={{
                            flex: 1,
                            padding: '8px',
                            backgroundColor: '#28a745',
                            color: 'white',
                            border: 'none',
                            borderRadius: '4px',
                            cursor: 'pointer',
                            fontWeight: 'bold',
                          }}
                        >
                          ✅ Confirmer
                        </button>
                        <button
                          onClick={() => setBorrowingBook(null)}
                          style={{
                            flex: 1,
                            padding: '8px',
                            backgroundColor: '#6c757d',
                            color: 'white',
                            border: 'none',
                            borderRadius: '4px',
                            cursor: 'pointer',
                          }}
                        >
                          ❌ Annuler
                        </button>
                      </div>
                    </div>
                  ) : (
                    <button
                      onClick={() => setBorrowingBook(book._id)}
                      style={{
                        width: '100%',
                        padding: '10px',
                        backgroundColor: '#007bff',
                        color: 'white',
                        border: 'none',
                        borderRadius: '4px',
                        cursor: 'pointer',
                        fontWeight: 'bold',
                      }}
                    >
                      📕 Emprunter
                    </button>
                  )}
                </div>
              )}

              {book.availableCopies === 0 && (
                <div
                  style={{
                    marginTop: '10px',
                    padding: '10px',
                    backgroundColor: '#ffe6e6',
                    color: '#d32f2f',
                    borderRadius: '4px',
                    textAlign: 'center',
                    fontWeight: 'bold',
                  }}
                >
                  ❌ Indisponible
                </div>
              )}
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

export default BookList
