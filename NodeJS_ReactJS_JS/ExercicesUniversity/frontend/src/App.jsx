import React from 'react';
import { AuthProvider, useAuth } from './context/AuthContext';
import HomePage from './pages/HomePage';
import DashboardPage from './pages/DashboardPage';

function AppContent() {
  const { user } = useAuth();

  return (
    <div style={{ fontFamily: 'Arial, sans-serif' }}>
      {user ? <DashboardPage /> : <HomePage />}
    </div>
  );
}

function App() {
  return (
    <AuthProvider>
      <AppContent />
    </AuthProvider>
  );
}

export default App;
