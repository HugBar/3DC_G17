// Author: Hugo Barros
// Frontend service for interacting with the allergy API endpoints
// Handles API calls for allergy management including adding, searching and updating allergies

import axios from 'axios';

// Set API base URL from environment variable or default to localhost
const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001';

/**
 * Service class that handles all allergy-related API calls
 * Provides methods for managing allergies including:
 * - Adding new allergies
 * - Searching existing allergies
 * - Updating allergy information
 */
class AllergyService {
    /**
     * Initialize service with base URL for allergy endpoints
     */
    constructor() {
        this.baseUrl = `${API_URL}/allergies`;
    }

    /**
     * Retrieves authentication token from local storage
     * @returns {string} The JWT auth token
     */
    getAuthToken() {
        return localStorage.getItem('authToken');
    }

    /**
     * Adds a new allergy to the system
     * @param {Object} allergyData - The allergy information to add
     * @returns {Promise<Object>} The created allergy data
     * @throws {Error} If the API call fails
     */
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

    /**
     * Searches for allergies based on provided parameters
     * @param {Object} searchParams - Search criteria for filtering allergies
     * @returns {Promise<Array>} Array of matching allergies
     * @throws {Error} If the API call fails
     */
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

    /**
     * Updates an existing allergy's information
     * @param {string} allergyId - ID of the allergy to update
     * @param {Object} allergyData - New allergy information
     * @returns {Promise<Object>} The updated allergy data
     * @throws {Error} If the API call fails
     */
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

// Create and export a singleton instance
const allergyService = new AllergyService();
export default allergyService;