import React from 'react';
import { useAuth } from '../context/AuthContext';
import Header from '../components/Header';
import BookList from '../components/BookList';
import MyBorrowsPage from './MyBorrowsPage';
import { useState } from 'react';

const DashboardPage = () => {
  const { user, token } = useAuth();
  const [activeTab, setActiveTab] = useState('books');

  if (!user || !token) {
    return <div style={{ padding: '20px', textAlign: 'center' }}>Please login first</div>;
  }

  return (
    <div>
      <Header />
      <div style={{ padding: '20px' }}>
        <div style={{ display: 'flex', gap: '10px', marginBottom: '20px' }}>
          <button
            onClick={() => setActiveTab('books')}
            style={{
              padding: '10px 20px',
              backgroundColor: activeTab === 'books' ? '#007bff' : '#ccc',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              cursor: 'pointer'
            }}
          >
            Browse Books
          </button>
          <button
            onClick={() => setActiveTab('myborrows')}
            style={{
              padding: '10px 20px',
              backgroundColor: activeTab === 'myborrows' ? '#007bff' : '#ccc',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              cursor: 'pointer'
            }}
          >
            My Borrows
          </button>
          {user.role === 'admin' && (
            <button
              onClick={() => setActiveTab('admin')}
              style={{
                padding: '10px 20px',
                backgroundColor: activeTab === 'admin' ? '#007bff' : '#ccc',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                cursor: 'pointer'
              }}
            >
              Admin Panel
            </button>
          )}
        </div>

        {activeTab === 'books' && <BookList />}
        {activeTab === 'myborrows' && <MyBorrowsPage />}
        {activeTab === 'admin' && user.role === 'admin' && <div><h2>Admin Panel</h2><p>Coming soon...</p></div>}
      </div>
    </div>
  );
};

export default DashboardPage;
