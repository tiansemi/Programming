import React, { createContext, useContext, useState, useEffect } from 'react'
import createLogger from '../utils/logger'

const logger = createLogger('AuthContext')
const AuthContext = createContext()

export const AuthProvider = ({ children }) => {
  const [user, setUser] = useState(null)
  const [token, setToken] = useState(localStorage.getItem('token'))
  const [loading, setLoading] = useState(false)

  // ✅ Au montage : charger token du localStorage
  useEffect(() => {
    logger.info('AuthProvider mounted, checking localStorage for token')
    const savedToken = localStorage.getItem('token')
    const savedUser = localStorage.getItem('user')
    
    if (savedToken) {
      logger.success('Token found in localStorage', { token: savedToken.substring(0, 20) + '...' })
      setToken(savedToken)
    } else {
      logger.debug('No token found in localStorage')
    }

    if (savedUser) {
      try {
        const parsedUser = JSON.parse(savedUser)
        logger.success('User data found in localStorage', parsedUser)
        setUser(parsedUser)
      } catch (error) {
        logger.error('Failed to parse user data from localStorage', error)
      }
    }
  }, [])

  // ✅ Sauvegarder token quand il change
  useEffect(() => {
    if (token) {
      logger.info('Token updated, saving to localStorage', { token: token.substring(0, 20) + '...' })
      localStorage.setItem('token', token)
    } else {
      logger.warn('Token removed, clearing localStorage')
      localStorage.removeItem('token')
    }
  }, [token])

  const login = (userData, authToken) => {
    logger.info('=== LOGIN CALLED ===')
    logger.debug('Login params:', { user: userData, token: authToken?.substring(0, 20) + '...' })
    
    setUser(userData)
    setToken(authToken)
    
    localStorage.setItem('user', JSON.stringify(userData))
    localStorage.setItem('token', authToken)
    
    logger.success('Login successful', {
      userId: userData?.id,
      username: userData?.username,
      role: userData?.role,
    })
  }

  const logout = () => {
    logger.warn('=== LOGOUT CALLED ===')
    
    setUser(null)
    setToken(null)
    localStorage.removeItem('token')
    localStorage.removeItem('user')
    
    logger.success('Logout successful')
  }

  return (
    <AuthContext.Provider value={{ user, token, loading, setLoading, login, logout }}>
      {children}
    </AuthContext.Provider>
  )
}

export const useAuth = () => {
  const context = useContext(AuthContext)
  if (!context) {
    throw new Error('useAuth must be used within AuthProvider')
  }
  return context
}
