import React, { useState } from 'react'
import { authService } from '../services/api'
import { useAuth } from '../context/AuthContext'
import createLogger from '../utils/logger'

const logger = createLogger('Login')

const Login = ({ onSuccess }) => {
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState('')
  const [loading, setLoading] = useState(false)
  const { login } = useAuth()

  const handleSubmit = async (e) => {
    e.preventDefault()
    logger.info('=== LOGIN FORM SUBMITTED ===', { email })
    
    setLoading(true)
    setError('')

    try {
      logger.debug('Sending login request to API', { email })
      const response = await authService.login(email, password)
      
      logger.success('API login response received', {
        user: response.data.user?.username,
        tokenLength: response.data.token?.length,
      })

      logger.debug('Calling context login()...', response.data)
      login(response.data.user, response.data.token)
      
      logger.success('Login context updated successfully')
      logger.debug('Calling onSuccess callback...')
      onSuccess?.()
    } catch (err) {
      const errorMsg = err.response?.data?.message || 'Login failed'
      logger.error('Login failed', err)
      setError(errorMsg)
    } finally {
      setLoading(false)
    }
  }

  return (
    <div style={{ maxWidth: '400px', margin: '50px auto', padding: '20px', border: '1px solid #ddd', borderRadius: '8px' }}>
      <h2>Login</h2>
      {error && <div style={{ color: 'red', marginBottom: '10px' }}>{error}</div>}
      <form onSubmit={handleSubmit}>
        <div style={{ marginBottom: '10px' }}>
          <input
            type="email"
            placeholder="Email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            required
            style={{ width: '100%', padding: '8px', boxSizing: 'border-box' }}
          />
        </div>
        <div style={{ marginBottom: '10px' }}>
          <input
            type="password"
            placeholder="Password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            required
            style={{ width: '100%', padding: '8px', boxSizing: 'border-box' }}
          />
        </div>
        <button
          type="submit"
          disabled={loading}
          style={{
            width: '100%',
            padding: '10px',
            backgroundColor: '#007bff',
            color: 'white',
            border: 'none',
            borderRadius: '4px',
            cursor: 'pointer',
          }}
        >
          {loading ? 'Logging in...' : 'Login'}
        </button>
      </form>
    </div>
  );
};

export default Login;
