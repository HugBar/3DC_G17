/**
 * Author: João Morais e Matias Vitorino
 * Frontend service for interacting with specializations API endpoints
 * Handles API calls for specialization management including creating, retrieving,
 * updating, deleting and searching specializations
 */

import axios from 'axios';

// Set API base URL from environment variable or default to localhost
const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001/api';

/**
 * Retrieves authentication token from local storage
 * @returns {string} The JWT auth token
 * @throws {Error} If no token is found
 */
const getAuthToken = () => {
    const token = localStorage.getItem('authToken');
    if (!token) {
        throw new Error('No auth token found');
    }
    return token;
};

const specializationService = {
    
    /**
     * Retrieves all specializations from the system
     * @returns {Promise<Array>} Array of all specializations
     * @throws {Error} If the API call fails
     */
    getAllSpecializations: async () => {
        try {
            const response = await axios.get(`${API_URL}/specializations`);
            return response.data;
        } catch (error) {
            console.error('Error fetching specializations:', error);
            throw error;
        }
    },

    /**
     * Creates a new specialization in the system
     * @param {Object} specializationData - The specialization data to add
     * @returns {Promise<Object>} The created specialization data
     * @throws {Error} If the API call fails
     */
    addSpecialization: async (specializationData) => {
        try {
            const response = await axios.post(`${API_URL}/specializations`, specializationData);
            return response.data;
        } catch (error) {
            console.error('Error adding specialization:', error);
            throw error;
        }
    },

    /**
     * Searches for specializations based on provided parameters
     * @param {Object} searchParams - Parameters to search by
     * @returns {Promise<Array>} Array of matching specializations
     * @throws {Error} If the search fails
     */
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

    /**
     * Deletes a specialization from the system
     * @param {string} id - The ID of the specialization to delete
     * @returns {Promise<Object>} The deleted specialization data
     * @throws {Error} When the deletion fails or unauthorized
     */
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
    },

    /**
     * Retrieves a specific specialization by its ID
     * @param {string} id - ID of the specialization to retrieve
     * @returns {Promise<Object>} The requested specialization data
     * @throws {Error} If the API call fails
     */
    getSpecializationById: async (id) => {
        try {
            const response = await axios.get(`${API_URL}/specializations/${id}`);
            return response.data;
        } catch (error) {
            console.error('Error fetching specialization:', error);
            throw error;
        }
    },

    /**
     * Updates an existing specialization
     * @param {string} id - ID of the specialization to update
     * @param {Object} specializationData - New specialization data
     * @returns {Promise<Object>} The updated specialization data
     * @throws {Error} If the update fails
     */
    updateSpecialization: async (id, specializationData) => {
        try {
            const response = await axios.put(`${API_URL}/specializations/${id}`, specializationData);
            return response.data;
        } catch (error) {
            console.error('Error updating specialization:', error);
            throw error;
        }
    }
};

export default specializationService;