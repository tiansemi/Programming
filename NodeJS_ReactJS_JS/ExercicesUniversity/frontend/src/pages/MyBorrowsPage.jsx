import React, { useState, useEffect } from 'react'
import { borrowService } from '../services/api'
import { useAuth } from '../context/AuthContext'
import createLogger from '../utils/logger'

const logger = createLogger('MyBorrowsPage')

const MyBorrowsPage = () => {
  const [borrows, setBorrows] = useState([])
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState('')
  const [success, setSuccess] = useState('')
  const { token } = useAuth()

  const fetchMyBorrows = React.useCallback(async () => {
    setLoading(true)
    try {
      logger.info('Fetching user borrows')
      const response = await borrowService.getUserBorrows()
      
      // ✅ Gérer correctement la réponse
      let borrowsData = response.data
      if (Array.isArray(borrowsData)) {
        setBorrows(borrowsData)
      } else if (borrowsData && typeof borrowsData === 'object') {
        setBorrows(borrowsData.borrows || borrowsData.data || [])
      } else {
        setBorrows([])
      }
      
      logger.success('Borrows loaded', { count: Array.isArray(borrowsData) ? borrowsData.length : 0 })
    } catch (error) {
      logger.error('Error fetching borrows', error)
      setError('Erreur lors du chargement de vos emprunts')
      setBorrows([])
    } finally {
      setLoading(false)
    }
  }, [])

  useEffect(() => {
    fetchMyBorrows()
  }, [fetchMyBorrows])

  const handleReturn = async (borrowId) => {
    try {
      logger.info('Returning book', { borrowId })
      await borrowService.returnBook(borrowId)
      logger.success('Book returned successfully')
      setSuccess('Livre retourné avec succès')
      fetchMyBorrows()
      setTimeout(() => setSuccess(''), 3000)
    } catch (error) {
      logger.error('Error returning book', error)
      setError('Erreur lors du retour du livre')
    }
  }

  // Calculer si un emprunt est en retard
  const isOverdue = (returnDate) => {
    return new Date() > new Date(returnDate)
  }

  // Grouper les emprunts par statut
  const borrowedBooks = borrows.filter((b) => b.status === 'borrowed')
  const returnedBooks = borrows.filter((b) => b.status === 'returned')
  const overdueBooks = borrowedBooks.filter((b) => isOverdue(b.returnDate))
  const activeBorrows = borrowedBooks.filter((b) => !isOverdue(b.returnDate))

  return (
    <div style={{ padding: '20px' }}>
      <h1>📋 Mes Emprunts</h1>

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

      {loading ? (
        <p>Chargement de vos emprunts...</p>
      ) : borrows.length === 0 ? (
        <p>Vous n'avez pas encore emprunté de livres.</p>
      ) : (
        <>
          {/* ⚠️ Emprunts en retard */}
          {overdueBooks.length > 0 && (
            <div style={{ marginBottom: '30px' }}>
              <h2 style={{ color: '#d32f2f' }}>⚠️ Emprunts en Retard ({overdueBooks.length})</h2>
              <table style={{ width: '100%', borderCollapse: 'collapse' }}>
                <thead>
                  <tr style={{ backgroundColor: '#f8d7da' }}>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Titre</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Auteur</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Emprunté le</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>À retourner avant</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'center' }}>Action</th>
                  </tr>
                </thead>
                <tbody>
                  {overdueBooks.map((borrow) => (
                    <tr key={borrow._id} style={{ backgroundColor: '#fff3cd' }}>
                      <td style={{ border: '1px solid #ddd', padding: '10px' }}>{borrow.book?.title}</td>
                      <td style={{ border: '1px solid #ddd', padding: '10px' }}>{borrow.book?.author}</td>
                      <td style={{ border: '1px solid #ddd', padding: '10px' }}>
                        {new Date(borrow.borrowDate).toLocaleDateString('fr-FR')}
                      </td>
                      <td style={{ border: '1px solid #ddd', padding: '10px', color: '#d32f2f', fontWeight: 'bold' }}>
                        {new Date(borrow.returnDate).toLocaleDateString('fr-FR')}
                      </td>
                      <td style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'center' }}>
                        <button
                          onClick={() => handleReturn(borrow._id)}
                          style={{
                            padding: '8px 15px',
                            backgroundColor: '#dc3545',
                            color: 'white',
                            border: 'none',
                            borderRadius: '4px',
                            cursor: 'pointer',
                            fontWeight: 'bold',
                          }}
                        >
                          🔙 Retourner
                        </button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          {/* 📕 Emprunts actifs */}
          {activeBorrows.length > 0 && (
            <div style={{ marginBottom: '30px' }}>
              <h2 style={{ color: '#007bff' }}>📕 Emprunts en Cours ({activeBorrows.length})</h2>
              <table style={{ width: '100%', borderCollapse: 'collapse' }}>
                <thead>
                  <tr style={{ backgroundColor: '#cfe2ff' }}>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Titre</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Auteur</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Emprunté le</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>À retourner avant</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'center' }}>Jours restants</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'center' }}>Action</th>
                  </tr>
                </thead>
                <tbody>
                  {activeBorrows.map((borrow) => {
                    const daysLeft = Math.ceil(
                      (new Date(borrow.returnDate) - new Date()) / (1000 * 60 * 60 * 24)
                    )
                    return (
                      <tr key={borrow._id} style={{ backgroundColor: '#f0f8ff' }}>
                        <td style={{ border: '1px solid #ddd', padding: '10px' }}>{borrow.book?.title}</td>
                        <td style={{ border: '1px solid #ddd', padding: '10px' }}>{borrow.book?.author}</td>
                        <td style={{ border: '1px solid #ddd', padding: '10px' }}>
                          {new Date(borrow.borrowDate).toLocaleDateString('fr-FR')}
                        </td>
                        <td style={{ border: '1px solid #ddd', padding: '10px' }}>
                          {new Date(borrow.returnDate).toLocaleDateString('fr-FR')}
                        </td>
                        <td style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'center', fontWeight: 'bold' }}>
                          {daysLeft > 0 ? `${daysLeft} j` : 'Aujourd\'hui'}
                        </td>
                        <td style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'center' }}>
                          <button
                            onClick={() => handleReturn(borrow._id)}
                            style={{
                              padding: '8px 15px',
                              backgroundColor: '#28a745',
                              color: 'white',
                              border: 'none',
                              borderRadius: '4px',
                              cursor: 'pointer',
                              fontWeight: 'bold',
                            }}
                          >
                            🔙 Retourner
                          </button>
                        </td>
                      </tr>
                    )
                  })}
                </tbody>
              </table>
            </div>
          )}

          {/* ✅ Livres retournés */}
          {returnedBooks.length > 0 && (
            <div>
              <h2 style={{ color: '#28a745' }}>✅ Historique ({returnedBooks.length})</h2>
              <table style={{ width: '100%', borderCollapse: 'collapse' }}>
                <thead>
                  <tr style={{ backgroundColor: '#d4edda' }}>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Titre</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Auteur</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Emprunté le</th>
                    <th style={{ border: '1px solid #ddd', padding: '10px', textAlign: 'left' }}>Retourné le</th>
                  </tr>
                </thead>
                <tbody>
                  {returnedBooks.map((borrow) => (
                    <tr key={borrow._id} style={{ backgroundColor: '#f1f9f5' }}>
                      <td style={{ border: '1px solid #ddd', padding: '10px' }}>{borrow.book?.title}</td>
                      <td style={{ border: '1px solid #ddd', padding: '10px' }}>{borrow.book?.author}</td>
                      <td style={{ border: '1px solid #ddd', padding: '10px' }}>
                        {new Date(borrow.borrowDate).toLocaleDateString('fr-FR')}
                      </td>
                      <td style={{ border: '1px solid #ddd', padding: '10px' }}>
                        {borrow.actualReturnDate
                          ? new Date(borrow.actualReturnDate).toLocaleDateString('fr-FR')
                          : new Date(borrow.returnDate).toLocaleDateString('fr-FR')}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </>
      )}
    </div>
  )
}

export default MyBorrowsPage
