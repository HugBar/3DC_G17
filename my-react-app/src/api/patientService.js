import axios from 'axios';
import { jwtDecode } from 'jwt-decode'; // Use named import

const API_URL = 'https://localhost:5001/api/patient';

const getAuthToken = () => {
  const token = localStorage.getItem('authToken');
  if (!token) {
    console.error('No auth token found');
    throw new Error('No auth token found');
  }
  return token;
};

const checkAdminRole = (token) => {
  const decodedToken = jwtDecode(token);

  // Access the role using the full claim key
  const role = decodedToken["http://schemas.microsoft.com/ws/2008/06/identity/claims/role"];
  if (role !== 'Admin') {
    console.error('User does not have the Admin role');
    throw new Error('User does not have the Admin role');
  }
};


const patientService = {
  getPatientProfile: async (patientEmail) => {
    const token = getAuthToken();
    try {
      const response = await axios.get(`${API_URL}/get-patient-profile?email=${patientEmail}`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error fetching patient profile:', error);
      throw error;
    }
  },

  updatePatientProfile: async (patientEmail, updateData) => {
    const token = getAuthToken();
    const patchData = Object.keys(updateData).map(key => ({
      op: "replace",
      path: `/${key}`,
      value: updateData[key]
    }));

    try {
      const response = await axios.patch(`${API_URL}/edit-patient-profile?email=${patientEmail}`, patchData, {
        headers: {
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json-patch+json',
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error updating patient profile:', error);
      throw error;
    }
  },

  getAllPatients: async (filters = {}, page = 1, pageSize = 5) => {
    const token = getAuthToken();
    checkAdminRole(token);

    try {
      const params = new URLSearchParams();
      Object.entries(filters).forEach(([key, value]) => {
        if (value) params.append(key, value);
      });
      params.append('page', page.toString());
      params.append('pageSize', pageSize.toString());
      
      const response = await axios.get(`${API_URL}/filter?${params.toString()}`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      
      console.log('API Response:', response.data);
      
      return response.data;
    } catch (error) {
      console.error('Error in getAllPatients:', error);
      throw error;
    }
  }
  
};

export default patientService;