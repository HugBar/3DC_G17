import axios from 'axios';

const API_URL = 'https://localhost:5001/api/auth';

export const login = async (credentials) => {
  try {
    const response = await axios.post(`${API_URL}/login`, credentials);
    return response.data.token; // Assuming the token is returned in the response
  } catch (error) {
    console.error('Error logging in:', error);
    throw error;
  }
}; 