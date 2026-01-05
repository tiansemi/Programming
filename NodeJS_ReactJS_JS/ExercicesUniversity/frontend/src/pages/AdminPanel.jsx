import React, { useState, useEffect } from 'react'
import { bookService, borrowService } from '../services/api'
import { useAuth } from '../context/AuthContext'
import createLogger from '../utils/logger'
import BookForm from '../components/BookForm'

const logger = createLogger('AdminPanel')

const AdminPanel = () => {
  const { token } = useAuth()
  const [activeTab, setActiveTab] = useState('books') // 'books' ou 'borrows'
  const [books, setBooks] = useState([])
  const [borrows, setBorrows] = useState([])
  const [loading, setLoading] = useState(false)
  const [showForm, setShowForm] = useState(false)
  const [editingBook, setEditingBook] = useState(null)
  const [error, setError] = useState('')
  const [success, setSuccess] = useState('')

  // ✅ Charger les livres
  useEffect(() => {
    if (activeTab === 'books') {
      fetchBooks()
    } else if (activeTab === 'borrows') {
      fetchAllBorrows()
    }
  }, [activeTab])

  const fetchBooks = async () => {
    setLoading(true)
    try {
      logger.info('Fetching all books for admin')
      const response = await bookService.getAllBooks('', '', 1, 100)
      setBooks(response.data.books || [])
      logger.success('Books loaded', { count: response.data.books?.length })
    } catch (err) {
      logger.error('Failed to fetch books', err)
      setError('Erreur lors du chargement des livres')
    } finally {
      setLoading(false)
    }
  }

  const fetchAllBorrows = async () => {
    setLoading(true)
    try {
      logger.info('Fetching all borrows for admin')
      const response = await borrowService.getAllBorrows('', 1, 100)
      
      // ✅ Gérer correctement la réponse (peut être un array ou un objet)
      let borrowsData = response.data
      if (Array.isArray(borrowsData)) {
        setBorrows(borrowsData)
      } else if (borrowsData && typeof borrowsData === 'object') {
        // Si c'est un objet, extraire le tableau (peut être response.data.borrows ou similar)
        setBorrows(borrowsData.borrows || borrowsData.data || [])
      } else {
        setBorrows([])
      }
      
      logger.success('Borrows loaded', { count: Array.isArray(borrowsData) ? borrowsData.length : (borrowsData?.borrows?.length || 0) })
    } catch (err) {
      logger.error('Failed to fetch borrows', err)
      setError('Erreur lors du chargement des emprunts')
      setBorrows([]) // Fallback to empty array
    } finally {
      setLoading(false)
    }
  }

  const handleAddBook = () => {
    logger.debug('Opening add book form')
    setEditingBook(null)
    setShowForm(true)
  }

  const handleEditBook = (book) => {
    logger.debug('Opening edit book form', { bookId: book._id })
    setEditingBook(book)
    setShowForm(true)
  }

  const handleDeleteBook = async (bookId) => {
    if (!window.confirm('Êtes-vous sûr de vouloir supprimer ce livre ?')) {
      return
    }

    try {
      logger.info('Deleting book', { bookId })
      await bookService.deleteBook(bookId, token)
      logger.success('Book deleted successfully')
      setSuccess('Livre supprimé avec succès')
      fetchBooks()
      setTimeout(() => setSuccess(''), 3000)
    } catch (err) {
      logger.error('Failed to delete book', err)
      setError('Erreur lors de la suppression du livre')
    }
  }

  const handleSaveBook = async (bookData) => {
    try {
      if (editingBook) {
        logger.info('Updating book', { bookId: editingBook._id })
        await bookService.updateBook(editingBook._id, bookData, token)
        logger.success('Book updated successfully')
        setSuccess('Livre mis à jour avec succès')
      } else {
        logger.info('Creating new book')
        await bookService.createBook(bookData, token)
        logger.success('Book created successfully')
        setSuccess('Livre créé avec succès')
      }
      setShowForm(false)
      fetchBooks()
      setTimeout(() => setSuccess(''), 3000)
    } catch (err) {
      logger.error('Failed to save book', err)
      setError(err.response?.data?.message || 'Erreur lors de la sauvegarde')
    }
  }

  return (
    <div style={{ padding: '20px' }}>
      <h1>🔧 Panel d'Administration</h1>

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

      {/* Navigation Tabs */}
      <div style={{ display: 'flex', gap: '10px', marginBottom: '20px', borderBottom: '2px solid #007bff' }}>
        <button
          onClick={() => setActiveTab('books')}
          style={{
            padding: '10px 20px',
            backgroundColor: activeTab === 'books' ? '#007bff' : '#e9ecef',
            color: activeTab === 'books' ? 'white' : 'black',
            border: 'none',
            borderRadius: '4px 4px 0 0',
            cursor: 'pointer',
            fontWeight: 'bold',
          }}
        >
          📚 Gestion des Livres
        </button>
        <button
          onClick={() => setActiveTab('borrows')}
          style={{
            padding: '10px 20px',
            backgroundColor: activeTab === 'borrows' ? '#007bff' : '#e9ecef',
            color: activeTab === 'borrows' ? 'white' : 'black',
            border: 'none',
            borderRadius: '4px 4px 0 0',
            cursor: 'pointer',
            fontWeight: 'bold',
          }}
        >
          📋 Tous les Emprunts
        </button>
      </div>

      {/* ONGLET: Gestion des Livres */}
      {activeTab === 'books' && (
        <div>
          <div style={{ marginBottom: '20px', display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
            <h2>Livres ({books.length})</h2>
            <button
              onClick={handleAddBook}
              style={{
                padding: '10px 20px',
                backgroundColor: '#28a745',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                cursor: 'pointer',
                fontWeight: 'bold',
              }}
            >
              + Ajouter un Livre
            </button>
          </div>

          {/* Formulaire d'ajout/édition */}
          {showForm && (
            <BookForm
              book={editingBook}
              onSave={handleSaveBook}
              onCancel={() => setShowForm(false)}
            />
          )}

          {/* Liste des livres */}
          {loading ? (
            <p>Chargement...</p>
          ) : books.length === 0 ? (
            <p>Aucun livre trouvé</p>
          ) : (
            <table style={{ width: '100%', borderCollapse: 'collapse', marginTop: '20px' }}>
              <thead>
                <tr style={{ backgroundColor: '#f5f5f5', borderBottom: '2px solid #ddd' }}>
                  <th style={{ padding: '10px', textAlign: 'left' }}>Titre</th>
                  <th style={{ padding: '10px', textAlign: 'left' }}>Auteur</th>
                  <th style={{ padding: '10px', textAlign: 'left' }}>Catégorie</th>
                  <th style={{ padding: '10px', textAlign: 'center' }}>Total</th>
                  <th style={{ padding: '10px', textAlign: 'center' }}>Disponible</th>
                  <th style={{ padding: '10px', textAlign: 'center' }}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {books.map((book) => (
                  <tr key={book._id} style={{ borderBottom: '1px solid #ddd' }}>
                    <td style={{ padding: '10px' }}>{book.title}</td>
                    <td style={{ padding: '10px' }}>{book.author}</td>
                    <td style={{ padding: '10px' }}>{book.category}</td>
                    <td style={{ padding: '10px', textAlign: 'center' }}>{book.totalCopies}</td>
                    <td style={{ padding: '10px', textAlign: 'center', fontWeight: 'bold', color: book.availableCopies === 0 ? 'red' : 'green' }}>
                      {book.availableCopies}
                    </td>
                    <td style={{ padding: '10px', textAlign: 'center' }}>
                      <button
                        onClick={() => handleEditBook(book)}
                        style={{
                          padding: '5px 10px',
                          marginRight: '5px',
                          backgroundColor: '#007bff',
                          color: 'white',
                          border: 'none',
                          borderRadius: '4px',
                          cursor: 'pointer',
                        }}
                      >
                        ✏️ Éditer
                      </button>
                      <button
                        onClick={() => handleDeleteBook(book._id)}
                        style={{
                          padding: '5px 10px',
                          backgroundColor: '#dc3545',
                          color: 'white',
                          border: 'none',
                          borderRadius: '4px',
                          cursor: 'pointer',
                        }}
                      >
                        🗑️ Supprimer
                      </button>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          )}
        </div>
      )}

      {/* ONGLET: Tous les Emprunts */}
      {activeTab === 'borrows' && (
        <div>
          <h2>Tous les Emprunts ({Array.isArray(borrows) ? borrows.length : 0})</h2>

          {loading ? (
            <p>Chargement...</p>
          ) : !Array.isArray(borrows) || borrows.length === 0 ? (
            <p>Aucun emprunt trouvé</p>
          ) : (
            <table style={{ width: '100%', borderCollapse: 'collapse', marginTop: '20px' }}>
              <thead>
                <tr style={{ backgroundColor: '#f5f5f5', borderBottom: '2px solid #ddd' }}>
                  <th style={{ padding: '10px', textAlign: 'left' }}>Utilisateur</th>
                  <th style={{ padding: '10px', textAlign: 'left' }}>Livre</th>
                  <th style={{ padding: '10px', textAlign: 'left' }}>Date Emprunt</th>
                  <th style={{ padding: '10px', textAlign: 'left' }}>Date Retour Prévue</th>
                  <th style={{ padding: '10px', textAlign: 'left' }}>Statut</th>
                </tr>
              </thead>
              <tbody>
                {Array.isArray(borrows) && borrows.length > 0 ? (
                  borrows.map((borrow) => (
                    <tr key={borrow._id} style={{ borderBottom: '1px solid #ddd' }}>
                      <td style={{ padding: '10px' }}>{borrow.user?.username || 'Utilisateur supprimé'}</td>
                      <td style={{ padding: '10px' }}>{borrow.book?.title || 'Livre supprimé'}</td>
                      <td style={{ padding: '10px' }}>
                        {new Date(borrow.borrowDate).toLocaleDateString('fr-FR')}
                      </td>
                      <td style={{ padding: '10px' }}>
                        {new Date(borrow.returnDate).toLocaleDateString('fr-FR')}
                      </td>
                      <td style={{ padding: '10px' }}>
                        <span
                          style={{
                            padding: '4px 8px',
                            borderRadius: '4px',
                            fontWeight: 'bold',
                            backgroundColor:
                              borrow.status === 'returned'
                                ? '#d4edda'
                                : borrow.status === 'overdue'
                                ? '#f8d7da'
                                : '#cfe2ff',
                            color:
                              borrow.status === 'returned'
                                ? '#155724'
                                : borrow.status === 'overdue'
                                ? '#721c24'
                                : '#004085',
                          }}
                        >
                          {borrow.status === 'returned'
                            ? '✅ Retourné'
                            : borrow.status === 'overdue'
                            ? '⚠️ En retard'
                            : '📕 Emprunté'}
                        </span>
                      </td>
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td colSpan="5" style={{ padding: '10px', textAlign: 'center' }}>
                      Aucun emprunt trouvé
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          )}
        </div>
      )}
    </div>
  )
}

export default AdminPanel
