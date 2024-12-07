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
}

const allergyService = new AllergyService();
export default allergyService;