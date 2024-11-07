import axios from 'axios';

const API_URL = 'https://localhost:5001/api/patient';

const getAuthToken = () => {
  const token = localStorage.getItem('authToken');
  if (!token) {
    console.error('No auth token found');
    throw new Error('No auth token found');
  }
  return token;
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
};

export default patientService;