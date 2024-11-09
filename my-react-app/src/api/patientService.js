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

  getAllPatients: async (filters = {}, page = 1, pageSize = 1) => {
    const token = getAuthToken();
    checkAdminRole(token);

    try {
      const params = new URLSearchParams();
      Object.entries(filters).forEach(([key, value]) => {
        if (value) params.append(key, value);
      });
      params.append('pageNumber', page.toString());
      params.append('pageSize', pageSize.toString());
      
      console.log('Request URL:', `${API_URL}/filter?${params.toString()}`); // Para debug
      
      const response = await axios.get(`${API_URL}/filter?${params.toString()}`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      
      console.log('Raw API Response:', response.data); // Para debug
      
      // Garantir que a resposta está no formato correto
      if (response.data && typeof response.data === 'object') {
        return {
          items: response.data.items || response.data,
          totalPages: response.data.totalPages || 0 ,
          currentPage: response.data.pageNumber || page,
          pageSize: response.data.pageSize || pageSize,
          totalCount: response.data.totalCount || 0
        };
      }
      
      return {
        items: [],
        totalPages: 1,
        currentPage: page,
        pageSize: pageSize,
        totalCount: 0
      };
    } catch (error) {
      console.error('Error in getAllPatients:', error);
      throw error;
    }
  },

  getPatientById: async (patientId) => {
    const token = getAuthToken();
    try {
      const response = await axios.get(`${API_URL}/get-patient-profile/${patientId}`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error fetching patient details:', error);
      throw error;
    }
  },

  deletePatient: async (patientId) => {
    const token = getAuthToken();
    checkAdminRole(token);
  
    const deleteDto = { ConfirmDeletion: true };
  
    try {
      const response = await axios.delete(`${API_URL}/delete-patient/${patientId}`, {
        headers: {
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
        data: deleteDto, 
      });
      return response.data;
    } catch (error) {
      console.error('Error deleting patient:', error.response?.data || error.message);
      throw error;
    }

  }
};

export default patientService;