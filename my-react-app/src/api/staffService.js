import axios from 'axios';

const API_URL = 'https://localhost:5001/api/staff';

const staffService = {
  createStaff: async (staffData) => {
    const token = localStorage.getItem('authToken');
    if (!token) {
      console.error('No auth token found');
      throw new Error('No auth token found');
    }

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
    const token = localStorage.getItem('authToken');
    if (!token) {
      console.error('No auth token found');
      throw new Error('No auth token found');
    }
  
    // Build JSON Patch for top-level fields
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
  
  
  
  

getAllStaff: async () => {
  const token = localStorage.getItem('authToken');
  if (!token) {
    console.error('No auth token found');
    throw new Error('No auth token found');
  }

  try {
    const response = await axios.get(`${API_URL}/filter`, {
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
  const token = localStorage.getItem('authToken');
  if (!token) {
    console.error('No auth token found');
    throw new Error('No auth token found');
  }

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


};

export default staffService;
