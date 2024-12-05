import axios from 'axios';

const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001/api';

const specializationService = {
    getAllSpecializations: async () => {
        try {
            const response = await axios.get(`${API_URL}/specializations`);
            return response.data;
        } catch (error) {
            console.error('Error fetching specializations:', error);
            throw error;
        }
    },

    addSpecialization: async (specializationData) => {
        try {
            const response = await axios.post(`${API_URL}/specializations`, specializationData);
            return response.data;
        } catch (error) {
            console.error('Error adding specialization:', error);
            throw error;
        }
    }
};

export default specializationService; 