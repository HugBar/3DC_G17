import axios from 'axios';
import { jwtDecode } from 'jwt-decode'; // Use named import

const API_URL = 'https://localhost:5001/api/OperationRequest/';

const getAuthToken = () => {
  const token = localStorage.getItem('authToken');
  if (!token) {
    console.error('No auth token found');
    throw new Error('No auth token found');
  }
  return token;
};

const checkDoctorRole = (token) => {
    const decodedToken = jwtDecode(token);
  
    // Access the role using the full claim key
    const role = decodedToken["http://schemas.microsoft.com/ws/2008/06/identity/claims/role"];
    if (role !== 'Doctor') {
      console.error('User does not have the Doctor role');
      throw new Error('User does not have the Doctor role');
    }
  };



const operationRequestService = {
  createOperationRequest: async (operationData) => {
    const token = getAuthToken();
    checkDoctorRole(token);

    try {
      const response = await axios.post(`${API_URL}create-operation-request`, operationData, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return response.data;
    } catch (error) {
      if (error.response) {
        console.error('Error response:', error.response.data);
        console.error('Error status:', error.response.status);
      } else if (error.request) {
        console.error('Error request:', error.request);
      } else {
        console.error('Error message:', error.message);
      }
      throw error;
    }
  },

  getAllOperationRequests: async () => {
    const token = getAuthToken();
    checkDoctorRole(token);
    try {
      const response = await axios.get(`${API_URL}filter`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return response.data;
    } catch (error) {
      if (error.response) {
        console.error('Error response:', error.response.data);
        console.error('Error status:', error.response.status);
      } else if (error.request) {
        console.error('Error request:', error.request);
      } else {
        console.error('Error message:', error.message);
      }
      throw error;
    }
  },
  deleteOperationRequest: async (id) => {
    const token = getAuthToken();
    checkDoctorRole(token);
    try {
      const response = await axios.delete(`${API_URL}delete-operation-request/${id}`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return response.data;
    } catch (error) {
      if (error.response) {
        console.error('Error response:', error.response.data);
        console.error('Error status:', error.response.status);
      } else if (error.request) {
        console.error('Error request:', error.request);
      } else {
        console.error('Error message:', error.message);
      }
      throw error;
    }
  },

  getOperationRequestById: async (id) => {
    const token = getAuthToken();
    checkDoctorRole(token);
    try {
      const response = await axios.get(`${API_URL}get/${id}`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return response.data;
    } catch (error) {
      if (error.response) {
        console.error('Error response:', error.response.data);
        console.error('Error status:', error.response.status);
      } else if (error.request) {
        console.error('Error request:', error.request);
      } else {
        console.error('Error message:', error.message);
      }
      throw error;
    }
  },
};

export default operationRequestService;
