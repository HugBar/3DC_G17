import axios from 'axios';

const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001';

class AllergyService {
    constructor() {
        this.baseUrl = `${API_URL}/allergies`;
    }

    getAuthToken() {
        return localStorage.getItem('authToken');
    }

    async addAllergy(allergyData) {
        try {
            const response = await axios.post(
                `${this.baseUrl}/add-allergy`,
                allergyData,
                {
                    headers: {
                        'Authorization': `Bearer ${this.getAuthToken()}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.error('Error details:', error.response || error);
            throw error;
        }
    }
    async searchAllergies(searchParams) {
        try {
                        
            const response = await axios.get(
                `${this.baseUrl}/search`,
                {
                    params: searchParams,
                    headers: {
                        'Authorization': `Bearer ${this.getAuthToken()}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            
            return response.data;
        } catch (error) {
            console.error('Erro detalhado:', {
                message: error.message,
                response: error.response?.data,
                status: error.response?.status
            });
            throw error;
        }
    }

    async updateAllergy(allergyId, allergyData) {
        try {
            const response = await axios.put(
                `${this.baseUrl}/update-allergy/${allergyId}`,
                allergyData);

            return response.data;
        }
        catch (error) {
            console.error('Error details:', error.response || error);
            throw error;
        }
    }

}

const allergyService = new AllergyService();
export default allergyService;