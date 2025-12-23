// test-api.js - Script pour tester l'API avec des appels HTTP
// À exécuter depuis la racine du backend avec : node test-api.js

const axios = require('axios');

const API_URL = 'http://localhost:5000/api';

let authToken = '';
let userId = '';
let bookId = '';

const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
};

const log = (type, message) => {
  const icons = {
    success: '✅',
    error: '❌',
    info: 'ℹ️',
    test: '🧪',
  };
  console.log(`${icons[type]} ${message}`);
};

const testRegister = async () => {
  try {
    log('test', 'Testing User Registration...');
    const response = await axios.post(`${API_URL}/auth/register`, {
      username: `testuser${Date.now()}`,
      email: `test${Date.now()}@example.com`,
      password: 'password123',
    });
    
    authToken = response.data.token;
    userId = response.data.user.id;
    
    log('success', 'User registered successfully');
    console.log(`Token: ${authToken.substring(0, 20)}...`);
    return true;
  } catch (error) {
    log('error', `Registration failed: ${error.response?.data?.message || error.message}`);
    return false;
  }
};

const testLogin = async () => {
  try {
    log('test', 'Testing User Login...');
    const response = await axios.post(`${API_URL}/auth/login`, {
      email: `test${userId}@example.com`,
      password: 'password123',
    });
    
    authToken = response.data.token;
    log('success', 'User logged in successfully');
    return true;
  } catch (error) {
    log('error', `Login failed: ${error.response?.data?.message || error.message}`);
    return false;
  }
};

const testGetProfile = async () => {
  try {
    log('test', 'Testing Get User Profile...');
    const response = await axios.get(`${API_URL}/auth/profile`, {
      headers: { Authorization: `Bearer ${authToken}` },
    });
    
    log('success', `Profile retrieved: ${response.data.username}`);
    return true;
  } catch (error) {
    log('error', `Get profile failed: ${error.response?.data?.message || error.message}`);
    return false;
  }
};

const testGetBooks = async () => {
  try {
    log('test', 'Testing Get All Books...');
    const response = await axios.get(`${API_URL}/books?page=1&limit=5`);
    
    if (response.data.books.length > 0) {
      bookId = response.data.books[0]._id;
      log('success', `Retrieved ${response.data.books.length} books`);
      console.log(`First book: ${response.data.books[0].title}`);
    } else {
      log('info', 'No books found in database');
    }
    return true;
  } catch (error) {
    log('error', `Get books failed: ${error.response?.data?.message || error.message}`);
    return false;
  }
};

const testSearchBooks = async () => {
  try {
    log('test', 'Testing Search Books...');
    const response = await axios.get(`${API_URL}/books/search?q=javascript`);
    
    log('success', `Search returned ${response.data.length} results`);
    return true;
  } catch (error) {
    log('error', `Search failed: ${error.response?.data?.message || error.message}`);
    return false;
  }
};

const testAddFavorite = async () => {
  if (!bookId) {
    log('error', 'No book ID available for testing favorites');
    return false;
  }

  try {
    log('test', 'Testing Add to Favorites...');
    const response = await axios.post(
      `${API_URL}/auth/favorites`,
      { bookId },
      { headers: { Authorization: `Bearer ${authToken}` } }
    );
    
    log('success', 'Book added to favorites');
    return true;
  } catch (error) {
    log('error', `Add favorite failed: ${error.response?.data?.message || error.message}`);
    return false;
  }
};

const testHealthCheck = async () => {
  try {
    log('test', 'Testing Server Health...');
    const response = await axios.get(`${API_URL.replace('/api', '')}/api/health`);
    
    log('success', 'Server is healthy');
    console.log(`Response: ${response.data.message}`);
    return true;
  } catch (error) {
    log('error', `Health check failed: ${error.message}`);
    return false;
  }
};

const runTests = async () => {
  console.log(
    `${colors.blue}${'='.repeat(50)}${colors.reset}`
  );
  console.log(`${colors.blue}🧪  API Test Suite${colors.reset}`);
  console.log(
    `${colors.blue}${'='.repeat(50)}${colors.reset}\n`
  );

  const tests = [
    { name: 'Health Check', fn: testHealthCheck },
    { name: 'User Registration', fn: testRegister },
    { name: 'Get User Profile', fn: testGetProfile },
    { name: 'Get All Books', fn: testGetBooks },
    { name: 'Search Books', fn: testSearchBooks },
    { name: 'Add to Favorites', fn: testAddFavorite },
  ];

  let passed = 0;
  let failed = 0;

  for (const test of tests) {
    const result = await test.fn();
    if (result) {
      passed++;
    } else {
      failed++;
    }
    console.log();
  }

  console.log(
    `${colors.blue}${'='.repeat(50)}${colors.reset}`
  );
  console.log(`${colors.green}Passed: ${passed}${colors.reset}`);
  console.log(`${colors.red}Failed: ${failed}${colors.reset}`);
  console.log(
    `${colors.blue}${'='.repeat(50)}${colors.reset}`
  );
};

// Run tests
runTests().catch(console.error);
