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
  console.log(role);
  if (role !== 'Admin') {
    console.error('User does not have the Admin role');
    throw new Error('User does not have the Admin role');
  }
};

const checkAdminDoctorRole = (token) => {
  const decodedToken = jwtDecode(token);

  // Access the role using the full claim key
  const role = decodedToken["http://schemas.microsoft.com/ws/2008/06/identity/claims/role"];
  if (role !== 'Doctor' && role !== 'Admin') {
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

  updateAdminPatientProfile: async (id, updateData) => {
    const token = getAuthToken();
    checkAdminRole(token);
    
    const patchData = [];
    
    if (updateData.phoneNumber !== undefined) {
        patchData.push({ op: "replace", path: "/phoneNumber", value: updateData.phoneNumber });
    }
    if (updateData.firstName !== undefined) {
        patchData.push({ op: "replace", path: "/firstName", value: updateData.firstName });
    }
    if (updateData.lastName !== undefined) {
        patchData.push({ op: "replace", path: "/lastName", value: updateData.lastName });
    }
    if (updateData.dateOfBirth !== undefined) {
        patchData.push({ op: "replace", path: "/dateOfBirth", value: updateData.dateOfBirth });
    }
    if (updateData.gender !== undefined) {
        patchData.push({ op: "replace", path: "/gender", value: updateData.gender });
    }
    if (updateData.emergencyContact !== undefined) {
        patchData.push({ op: "replace", path: "/emergencyContact", value: updateData.emergencyContact });
    }
    if (updateData.medicalHistory !== undefined) {
        patchData.push({ op: "replace", path: "/medicalHistory", value: updateData.medicalHistory });
    }

    try {
        const response = await axios.patch(`${API_URL}/admin/edit-patient-profile/${id}`, patchData, {
            headers: {
                Authorization: `Bearer ${token}`,
                'Content-Type': 'application/json-patch+json',
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


  getAllPatients: async (filters = {}, page = 1, pageSize = 1) => {
    const token = getAuthToken();
    checkAdminDoctorRole(token);

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

  },
  requestAccountDeletion: async () => {
    const token = getAuthToken();
    try {
      const response = await axios.post(`${API_URL}/account-deletion-request`, null, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error requesting account deletion:', error);
      throw error;
    }
  },

  confirmAccountDeletion: async (token) => {
    const authToken = getAuthToken();
    try {
      const response = await axios.delete(`${API_URL}/confirm-account-deletion`, {
        headers: {
          Authorization: `Bearer ${authToken}`,
        },
        data: { token }
      });
      return response.data;
    } catch (error) {
      console.error('Error confirming account deletion:', error);
      throw error;
    }
  },
  
  registerPatient: async (patientData) => {
    const token = getAuthToken();
    checkAdminRole(token);

    try {
        const response = await axios.post(API_URL, patientData, {
            headers: {
                Authorization: `Bearer ${token}`,
            }
        });
        
        if (response.data) {
            console.log('Patient registered successfully:', response.data);
            return response.data;
        }
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

registerPatientItself: async (patientData) => {
  const token = getAuthToken();

  try {
      const response = await axios.post(API_URL, patientData, {
          headers: {
              Authorization: `Bearer ${token}`,
          }
      });
      
      if (response.data) {
          console.log('Patient registered successfully:', response.data);
          return response.data;
      }
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

export default patientService;