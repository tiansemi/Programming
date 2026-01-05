import React from 'react'
import { useAuth } from '../context/AuthContext'
import createLogger from '../utils/logger'
import Header from '../components/Header'
import BookList from '../components/BookList'
import MyBorrowsPage from './MyBorrowsPage'
import AdminPanel from './AdminPanel'
import { useState } from 'react'

const logger = createLogger('DashboardPage')

const DashboardPage = () => {
  const { user, token } = useAuth()
  const [activeTab, setActiveTab] = useState('books')

  if (!user || !token) {
    return <div style={{ padding: '20px', textAlign: 'center' }}>Veuillez vous connecter d'abord</div>
  }

  logger.debug('Dashboard rendered', { username: user.username, role: user.role, isAdmin: user.role === 'admin' })

  return (
    <div>
      <Header />
      <div style={{ padding: '20px' }}>
        <div style={{ display: 'flex', gap: '10px', marginBottom: '20px', borderBottom: '2px solid #007bff' }}>
          <button
            onClick={() => {
              logger.debug('Switching to books tab')
              setActiveTab('books')
            }}
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
            📚 Parcourir les Livres
          </button>
          <button
            onClick={() => {
              logger.debug('Switching to myborrows tab')
              setActiveTab('myborrows')
            }}
            style={{
              padding: '10px 20px',
              backgroundColor: activeTab === 'myborrows' ? '#007bff' : '#e9ecef',
              color: activeTab === 'myborrows' ? 'white' : 'black',
              border: 'none',
              borderRadius: '4px 4px 0 0',
              cursor: 'pointer',
              fontWeight: 'bold',
            }}
          >
            📋 Mes Emprunts
          </button>
          {user.role === 'admin' && (
            <button
              onClick={() => {
                logger.debug('Switching to admin tab')
                setActiveTab('admin')
              }}
              style={{
                padding: '10px 20px',
                backgroundColor: activeTab === 'admin' ? '#dc3545' : '#e9ecef',
                color: activeTab === 'admin' ? 'white' : 'black',
                border: 'none',
                borderRadius: '4px 4px 0 0',
                cursor: 'pointer',
                fontWeight: 'bold',
              }}
            >
              🔧 Panel Admin
            </button>
          )}
        </div>

        {activeTab === 'books' && <BookList />}
        {activeTab === 'myborrows' && <MyBorrowsPage />}
        {activeTab === 'admin' && user.role === 'admin' && <AdminPanel />}
      </div>
    </div>
  )
}

export default DashboardPage
