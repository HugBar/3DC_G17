import axios from 'axios';

const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001';

const getAuthToken = () => {
    const token = localStorage.getItem('authToken');
    if (!token) {
        console.error('No auth token found');
        throw new Error('No auth token found');
    }
    return token;
};

const medicalRecordService = {
    getMedicalRecord: async (patientId) => {
        const token = getAuthToken();
        try {
            const response = await axios.get(`${API_URL}/medical-records/${patientId}`, {
                headers: { Authorization: `Bearer ${token}` }
            });
            return response.data;
        } catch (error) {
            throw error;
        }
    },

    getAllMedicalConditions: async () => {
        const token = getAuthToken();
        try {
            const response = await axios.get(`${API_URL}/medical-conditions/getConditionDetails`, {
                headers: { Authorization: `Bearer ${token}` }
            });
            return response.data;
        } catch (error) {
            throw error;
        }
    },

    getAllAllergies: async () => {
        const token = getAuthToken();
        try {
            const response = await axios.get(`${API_URL}/allergies/getAllergyDetails`, {
                headers: { Authorization: `Bearer ${token}` }
            });
            return response.data;
        } catch (error) {
            console.error('Error fetching allergies:', error.response || error);
            throw error;
        }
    },

    updateMedicalRecord: async (patientId, updateData) => {
        const token = getAuthToken();
        try {
            const response = await axios.put(
                `${API_URL}/medical-records/update/${patientId}`,
                updateData,
                {
                    headers: { 
                        Authorization: `Bearer ${token}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            return response.data;
        } catch (error) {
            throw error;
        }
    },

    searchMedicalRecord: async (patientId, conditionName, allergyName) => {
        try {
            let url = `${API_URL}/medical-records/search?patientId=${patientId}`;
            if (conditionName) url += `&conditionName=${conditionName}`;
            if (allergyName) url += `&allergyName=${allergyName}`;
            
            const response = await axios.get(url, {
                headers: { Authorization: `Bearer ${getAuthToken()}` }
            });
            return response.data;
        } catch (error) {
            throw error;
        }
    }
};

export default medicalRecordService;