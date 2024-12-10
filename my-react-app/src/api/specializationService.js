import axios from 'axios';

const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001/api';

const getAuthToken = () => {
    const token = localStorage.getItem('authToken');
    if (!token) {
        throw new Error('No auth token found');
    }
    return token;
};

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
    },

    searchSpecializations: async (searchParams) => {
        try {
            const response = await axios.get(
                `${API_URL}/specializations/search`,
                {
                    params: searchParams
                }
            );
            console.log('Search response:', response.data); // Log the response
            return response.data;
        } catch (error) {
            console.error('Search error details:', {
                data: error.response?.data,
                status: error.response?.status
            });
            throw error;
        }
    },
    
    deleteSpecialization: async (id) => {
        const token = getAuthToken();
        try {
            const response = await axios.delete(`${API_URL}/specializations/${id}`, {
                headers: {
                    'Authorization': `Bearer ${token}`,
                    'Content-Type': 'application/json'
                }
            });
            return response.data;
        } catch (error) {
            console.error('Error deleting specialization:', error);
            throw error;
        }
    }
};



export default specializationService;