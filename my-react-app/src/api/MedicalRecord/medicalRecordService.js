/**
 * Author: Hugo Barros
 * Frontend service for interacting with medical records API endpoints
 * Handles API calls for medical record management including retrieving, updating,
 * and searching patient medical records, conditions and allergies
 */

import axios from 'axios';

// Set API base URL from environment variable or default to localhost
const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001';

/**
 * Retrieves authentication token from local storage
 * @returns {string} The JWT auth token
 * @throws {Error} If no token is found
 */
const getAuthToken = () => {
    const token = localStorage.getItem('authToken');
    if (!token) {
        console.error('No auth token found');
        throw new Error('No auth token found');
    }
    return token;
};

const medicalRecordService = {
    /**
     * Retrieves a patient's medical record by their ID
     * @param {string} patientId - ID of the patient
     * @returns {Promise<Object>} The patient's medical record data
     * @throws {Error} If the API call fails
     */
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

    /**
     * Retrieves all medical conditions from the system
     * @returns {Promise<Array>} Array of all medical conditions
     * @throws {Error} If the API call fails
     */
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

    /**
     * Retrieves all allergies from the system
     * @returns {Promise<Array>} Array of all allergies
     * @throws {Error} If the API call fails
     */
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

    /**
     * Updates a patient's medical record
     * @param {string} patientId - ID of the patient
     * @param {Object} updateData - New medical record data
     * @returns {Promise<Object>} The updated medical record
     * @throws {Error} If the API call fails
     */
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

    /**
     * Searches for medical records based on patient ID and optional filters
     * @param {string} patientId - ID of the patient
     * @param {string} [conditionName] - Optional medical condition to filter by
     * @param {string} [allergyName] - Optional allergy to filter by
     * @returns {Promise<Array>} Array of matching medical records
     * @throws {Error} If the API call fails
     */
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
    },

    /**
     * Verifies if a patient exists in the system
     * @param {string} patientId - ID of the patient to verify
     * @returns {Promise<boolean>} True if patient exists, false otherwise
     */
    verifyPatient: async (patientId) => {
        try {
            const response = await axios.get(`https://localhost:5001/api/patient/verify/${patientId}`, {
                headers: {
                    Authorization: `Bearer ${getAuthToken()}`
                }
            });
            return response.status === 200;
        } catch (error) {
            return false;
        }
    }
};

export default medicalRecordService;