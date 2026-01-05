import React, { useEffect } from 'react'
import { AuthProvider, useAuth } from './context/AuthContext'
import createLogger from './utils/logger'
import HomePage from './pages/HomePage'
import DashboardPage from './pages/DashboardPage'

const logger = createLogger('App')

function AppContent() {
  const { user, token, loading } = useAuth()

  useEffect(() => {
    logger.info('=== APP RENDERED ===', {
      isAuthenticated: !!token,
      user: user?.username || 'No user',
      loading,
    })
  }, [user, token, loading])

  if (loading) {
    return <div style={{ padding: '20px', textAlign: 'center' }}>Chargement...</div>
  }

  logger.debug(`Current auth state:`, {
    token: token ? token.substring(0, 20) + '...' : 'No token',
    user: user?.username || 'Not authenticated',
  })

  return (
    <div style={{ fontFamily: 'Arial, sans-serif' }}>
      {token && user ? (
        <>
          {logger.success('User authenticated, showing DashboardPage')}
          <DashboardPage />
        </>
      ) : (
        <>
          {logger.warn('User not authenticated, showing HomePage')}
          <HomePage />
        </>
      )}
    </div>
  )
}

function App() {
  useEffect(() => {
    logger.info('🚀 Application started')
  }, [])

  return (
    <AuthProvider>
      <AppContent />
    </AuthProvider>
  )
}

export default App
