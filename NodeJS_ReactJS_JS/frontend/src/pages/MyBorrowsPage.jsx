import React, { useState, useEffect } from 'react';
import { borrowService } from '../services/api';
import { useAuth } from '../context/AuthContext';

const MyBorrowsPage = () => {
  const [borrows, setBorrows] = useState([]);
  const [loading, setLoading] = useState(false);
  const { token } = useAuth();

  const fetchMyBorrows = React.useCallback(async () => {
    setLoading(true);
    try {
      const response = await borrowService.getUserBorrows(token);
      setBorrows(response.data);
    } catch (error) {
      console.error('Error fetching borrows:', error);
    } finally {
      setLoading(false);
    }
  }, [token]);

  useEffect(() => {
    fetchMyBorrows();
  }, [fetchMyBorrows]);

  const handleReturn = async (borrowId) => {
    try {
      await borrowService.returnBook(borrowId, token);
      fetchMyBorrows();
    } catch (error) {
      console.error('Error returning book:', error);
    }
  };

  return (
    <div style={{ padding: '20px' }}>
      <h2>My Borrowed Books</h2>
      
      {loading ? (
        <p>Loading...</p>
      ) : borrows.length === 0 ? (
        <p>You haven't borrowed any books yet.</p>
      ) : (
        <table style={{ width: '100%', borderCollapse: 'collapse' }}>
          <thead>
            <tr style={{ backgroundColor: '#f5f5f5' }}>
              <th style={{ border: '1px solid #ddd', padding: '10px' }}>Book Title</th>
              <th style={{ border: '1px solid #ddd', padding: '10px' }}>Author</th>
              <th style={{ border: '1px solid #ddd', padding: '10px' }}>Borrow Date</th>
              <th style={{ border: '1px solid #ddd', padding: '10px' }}>Return Date</th>
              <th style={{ border: '1px solid #ddd', padding: '10px' }}>Status</th>
              <th style={{ border: '1px solid #ddd', padding: '10px' }}>Action</th>
            </tr>
          </thead>
          <tbody>
            {borrows.map((borrow) => (
              <tr key={borrow._id}>
                <td style={{ border: '1px solid #ddd', padding: '10px' }}>{borrow.book?.title}</td>
                <td style={{ border: '1px solid #ddd', padding: '10px' }}>{borrow.book?.author}</td>
                <td style={{ border: '1px solid #ddd', padding: '10px' }}>{new Date(borrow.borrowDate).toLocaleDateString()}</td>
                <td style={{ border: '1px solid #ddd', padding: '10px' }}>{new Date(borrow.returnDate).toLocaleDateString()}</td>
                <td style={{ border: '1px solid #ddd', padding: '10px' }}>
                  <span style={{
                    padding: '5px 10px',
                    backgroundColor: borrow.status === 'returned' ? '#28a745' : '#ffc107',
                    color: 'white',
                    borderRadius: '4px'
                  }}>
                    {borrow.status}
                  </span>
                </td>
                <td style={{ border: '1px solid #ddd', padding: '10px' }}>
                  {borrow.status === 'borrowed' && (
                    <button
                      onClick={() => handleReturn(borrow._id)}
                      style={{
                        padding: '5px 10px',
                        backgroundColor: '#dc3545',
                        color: 'white',
                        border: 'none',
                        borderRadius: '4px',
                        cursor: 'pointer'
                      }}
                    >
                      Return
                    </button>
                  )}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
    </div>
  );
};

export default MyBorrowsPage;
