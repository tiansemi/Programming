const bcrypt = require('bcryptjs')
const mongoose = require('mongoose')
require('dotenv').config()

const User = require('./models/User')

async function createAdmin() {
  try {
    await mongoose.connect(process.env.MONGODB_URI)
    
    // ⚠️ NE PAS hacher ici - Mongoose le fera automatiquement !
    const admin = await User.create({
      username: 'admin2',
      email: 'admin@example.com',
      password: 'Admin@123',  // Mot de passe en clair - Mongoose le hashera
      role: 'admin'
    })
    
    console.log('✅ Admin créé avec succès :', admin)
    console.log('Email: admin@library.com')
    console.log('Password: admin123')
    process.exit(0)
  } catch (error) {
    console.error('❌ Erreur :', error.message)
    process.exit(1)
  }
}

createAdmin()