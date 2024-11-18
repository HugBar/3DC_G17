import axios from 'axios';

const API_URL = 'https://localhost:5001/api/auth';
const GOOGLE_AUTH_URL = 'https://localhost:5001/api/GoogleAuth';

export const login = async (credentials) => {
  try {
    const response = await axios.post(`${API_URL}/login`, credentials);
    return response.data.token; // Assuming the token is returned in the response
  } catch (error) {
    console.error('Error logging in:', error);
    throw error;
  }
};

export const initiateGoogleLogin = () => {
  window.location.href = `${GOOGLE_AUTH_URL}/google-login`;
};

export const handleGoogleCallback = async (token) => {
  if (!token) {
    throw new Error('No token received');
  }
  
  localStorage.setItem('authToken', token);
  return token;
};