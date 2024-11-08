import axios from 'axios';
import { jwtDecode } from 'jwt-decode'; // Use named import

const API_URL = 'https://localhost:5001/api/staff';

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

const staffService = {
  createStaff: async (staffData) => {
    const token = getAuthToken();
    checkAdminRole(token);

    try {
      const response = await axios.post(`${API_URL}/create-staff-profile`, staffData, {
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

  updateStaff: async (staffId, updateData) => {
    const token = getAuthToken();
    checkAdminRole(token);

    const patchData = [];
    if (updateData.email !== undefined) {
      patchData.push({ op: "replace", path: "/email", value: updateData.email });
    }
    if (updateData.phoneNumber !== undefined) {
      patchData.push({ op: "replace", path: "/phoneNumber", value: updateData.phoneNumber });
    }
    if (updateData.specialization !== undefined) {
      patchData.push({ op: "replace", path: "/specialization", value: updateData.specialization });
    }
    if (Array.isArray(updateData.availabilitySlots)) {
      patchData.push({
        op: "replace",
        path: "/availabilitySlots",
        value: updateData.availabilitySlots.map(slot => ({
          startTime: slot.startTime,
          endTime: slot.endTime
        }))
      });
    }

    try {
      const response = await axios.patch(`${API_URL}/edit-staff-profile/${staffId}`, patchData, {
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

  getAllStaff: async (filters = {}) => {
    const token = getAuthToken();
    checkAdminRole(token);

    try {
      // Convert filters object to URL parameters
      const params = new URLSearchParams();
      Object.entries(filters).forEach(([key, value]) => {
        if (value) params.append(key, value);
      });

      const response = await axios.get(`${API_URL}/filter?${params.toString()}`, {
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

  getStaffById: async (staffId) => {
    const token = getAuthToken();

    try {
      const response = await axios.get(`${API_URL}/get-staff-profile/${staffId}`, {
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
  deactivateStaff: async (staffId) => {
    const token = getAuthToken();
    checkAdminRole(token);

    try {
      const response = await axios.patch(`${API_URL}/${staffId}/deactivate`, null, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return response.data;
    } catch (error) {
      if (error.response) {
        console.error('Error response:', error.response.data);
        console.error('Error status:', error.response.status);
        console.error('Error headers:', error.response.headers);
      } else if (error.request) {
        console.error('Error request:', error.request);
      } else {
        console.error('Error message:', error.message);
      }
      throw error;
    }
  },

  getDeactivatedStaff: async () => {
    const token = getAuthToken();
    checkAdminRole(token);

    try {
      const response = await axios.get(`${API_URL}/deactivated-staffs`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return response.data;
    } catch (error) {
      throw error;
    }
  }

};

export default staffService;
