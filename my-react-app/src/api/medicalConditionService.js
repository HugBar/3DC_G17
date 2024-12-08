import axios from 'axios';

const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001';

class MedicalConditionService {
    constructor() {
        this.baseUrl = `${API_URL}/medical-conditions`;
    }

    getAuthToken() {
        return localStorage.getItem('authToken');
    }

    async addMedicalCondition(medicalConditionData) {
        try {
            const response = await axios.post(
                `${this.baseUrl}/add-medical-condition`,
                medicalConditionData,
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

const medicalConditionService = new MedicalConditionService();
export default medicalConditionService;