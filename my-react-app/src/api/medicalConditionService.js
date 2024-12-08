import axios from 'axios';

const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001';

class MedicalConditionService {
    constructor() {
        this.baseUrl = `${API_URL}/medical-conditions`;
    }

    getAuthToken() {
        return localStorage.getItem('authToken');
    }

    async addMedicalCondition(medicalConditionRequest) {
        try {
            const response = await axios.post(
                `${this.baseUrl}/add-medical-condition`,
                medicalConditionRequest,
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
    
    async searchMedicalConditions(searchParams) {
        try {
            console.log('Enviando requisição com params:', searchParams); // Debug
            
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
            
            console.log('Resposta recebida:', response.data); // Debug
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

}

const medicalConditionService = new MedicalConditionService();
export default medicalConditionService;