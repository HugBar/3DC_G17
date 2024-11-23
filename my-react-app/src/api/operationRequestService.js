import axios from 'axios';
import { jwtDecode } from 'jwt-decode'; // Use named import
import staffService from './staffService'; 
import patientService from './patientService'; 

const API_URL = 'https://localhost:5001/api/OperationRequest/';
const Type_API_URL = 'https://localhost:5001/api/OperationType/';

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
  updateOperationRequest: async (id, updateData) => {
    const token = getAuthToken();
    checkDoctorRole(token); // Changed to doctor role since operation requests are managed by doctors
    
    const patchData = [];
    
    // Change fields to match OperationRequest structure
    if (updateData.operationTypeId !== undefined) {
      patchData.push({ 
        op: "replace", 
        path: "/operationTypeId", 
        value: updateData.operationTypeId 
      });
    }
    if (updateData.deadline !== undefined) {
      patchData.push({ 
        op: "replace", 
        path: "/deadline", 
        value: updateData.deadline 
      });
    }
    if (updateData.priority !== undefined) {
      patchData.push({ 
        op: "replace", 
        path: "/priority", 
        value: updateData.priority 
      });
    }
  
    try {
      const response = await axios.patch(`${API_URL}${id}`, patchData, {
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
  
  

  getAllOperationRequests: async (filters = {}) => {
    const token = getAuthToken();
    checkDoctorRole(token);

    try {
      // Convert filters object to URL parameters
      const params = new URLSearchParams();
      Object.entries(filters).forEach(([key, value]) => {
        if (value) params.append(key, value);
      });

      const response = await axios.get(`${API_URL}filter?${params.toString()}`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });

      const operationRequests = response.data;

      // Fetch doctor details for each operation request
      const operationRequestsWithDetails = await Promise.all(
        operationRequests.map(async (request) => {
          let doctorLicenseNumber = '';
          let doctorName = '';
          let patientMedicalNumber = '';

          try {
            const doctorDetails = await staffService.getStaffById(request.doctorId);
            doctorLicenseNumber = doctorDetails.licenseNumber;
            doctorName = `${doctorDetails.firstName} ${doctorDetails.lastName}`;
          } catch (error) {
            console.error(`Error fetching doctor details for doctorId ${request.doctorId}:`, error);
          }

          try {
            const patientDetails = await patientService.getPatientById(request.patientId);
            patientMedicalNumber = patientDetails.medicalNr;
          } catch (error) {
            console.error(`Error fetching patient details for patientId ${request.patientId}:`, error);
          }

          return {
            ...request,
            doctorLicenseNumber,
            doctorName,
            patientMedicalNumber,
          };
        })
      );

      return operationRequestsWithDetails;
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

  getAllOperationTypes: async () => {
    const token = getAuthToken();
    try {
      const response = await axios.get(`${Type_API_URL}search-operation-type`, {
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
