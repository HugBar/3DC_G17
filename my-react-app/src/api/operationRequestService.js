import axios from 'axios';

const API_URL = 'https://localhost:5001/api/OperationRequest/';

const getAuthToken = () => {
  const token = localStorage.getItem('authToken');
  if (!token) {
    console.error('No auth token found');
    throw new Error('No auth token found');
  }
  return token;
};

const operationRequestService = {
  createOperationRequest: async (operationData) => {
    const token = getAuthToken();

    try {
      const response = await axios.post(`${API_URL}/create-operation-request`, operationData, {
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