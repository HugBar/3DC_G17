import axios from 'axios';

const API_URL = 'https://localhost:5001/api/operationtype';


const operationTypeService = {
    getAllOperationTypes: async () => {
        const token = localStorage.getItem('authToken');
        try {
            const response = await axios.get(`${API_URL}/search-operation-type`, {
                headers: { Authorization: `Bearer ${token}` }
            });
            return response.data;
        } catch (error) {
            console.error('Error fetching operation types:', error);
            throw error;
        }
    },
    updateOperationType: async (id, operationType) => {
        const token = localStorage.getItem('authToken');
        try {
            if (!id) {
                throw new Error('Invalid operation type ID');
            }

            const response = await axios.patch(
                `${API_URL}/update-specializations/${id}`, 
                {
                    requiredStaffBySpecialization: operationType.requiredStaffBySpecialization
                },
                {
                    headers: { 
                        Authorization: `Bearer ${token}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.log('Full error object:', error);
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
    }
};

export default operationTypeService; 