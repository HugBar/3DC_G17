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

export const googleLogin = () => {
  try {
    // Redirect to the Google login endpoint
    window.location.href = `${GOOGLE_AUTH_URL}/google-login`;
  } catch (error) {
    console.error('Error initiating Google login:', error);
    throw error;
  }
};

// Optional: Add a method to handle the Google response
export const handleGoogleResponse = async (code) => {
  try {
    const response = await axios.get(`${GOOGLE_AUTH_URL}/google-response?code=${code}`);
    return response.data;
  } catch (error) {
    console.error('Error handling Google response:', error);
    throw error;
  }
};
