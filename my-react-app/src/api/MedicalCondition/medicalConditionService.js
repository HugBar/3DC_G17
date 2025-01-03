// Author: Matias Vitorino

import axios from 'axios';

const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001';

class MedicalConditionService {
    constructor() {
        this.baseUrl = `${API_URL}/medical-conditions`;
    }

    getAuthToken() {
        return localStorage.getItem('authToken');
    }
    /**
     * This function adds a new medical condition to the system.
     * @param {Object} medicalConditionRequest - The medical condition data to add.
     * @returns {Promise} A promise that resolves with the added medical condition data.
     * @throws {Error} When the addition fails 
     */
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
    
    /**
     * This function searches for medical conditions in the system based on the provided parameters.
     * @param {Object} searchParams - The search parameters to use.
     * @returns {Promise} A promise that resolves with the search results.
     * @throws {Error} When the search fails.
     */
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
    /**
     * 
     * This funtion is responsable for updating an medical condition in the system
     */
    async updateMedicalCondition(conditionId, conditionData) {
        try {
            const response = await axios.put(
                `${this.baseUrl}/update/${conditionId}`,  // Mudou o endpoint
                conditionData,
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

    async getMedicalCondition(id) {
        try {
            const response = await axios.get(
                `${this.baseUrl}/${id}`,
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