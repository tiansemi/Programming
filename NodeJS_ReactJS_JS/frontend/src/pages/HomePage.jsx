import React, { useState } from 'react';
import { useAuth } from '../context/AuthContext';
import Login from '../components/Login';
import Register from '../components/Register';

const HomePage = () => {
  const { user } = useAuth();
  const [showLogin, setShowLogin] = useState(true);

  if (user) {
    return null; // Redirect to main app
  }

  return (
    <div style={{ padding: '40px 20px', textAlign: 'center' }}>
      <h1>Welcome to Library Management System 📚</h1>
      <p>A modern library management application built with Node.js and React.js</p>
      
      <div style={{ display: 'flex', justifyContent: 'center', gap: '10px', marginBottom: '30px' }}>
        <button
          onClick={() => setShowLogin(true)}
          style={{
            padding: '10px 20px',
            backgroundColor: showLogin ? '#007bff' : '#ccc',
            color: 'white',
            border: 'none',
            borderRadius: '4px',
            cursor: 'pointer'
          }}
        >
          Login
        </button>
        <button
          onClick={() => setShowLogin(false)}
          style={{
            padding: '10px 20px',
            backgroundColor: !showLogin ? '#28a745' : '#ccc',
            color: 'white',
            border: 'none',
            borderRadius: '4px',
            cursor: 'pointer'
          }}
        >
          Register
        </button>
      </div>

      {showLogin ? (
        <Login onSuccess={() => window.location.reload()} />
      ) : (
        <Register onSuccess={() => setShowLogin(true)} />
      )}
    </div>
  );
};

export default HomePage;
