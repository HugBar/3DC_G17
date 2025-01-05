/**
 * Author: Hugo Barros
 * Frontend service for interacting with the surgery appointments API endpoints
 * Handles API calls for appointment management including creating, updating, searching
 * and checking availability of surgery appointments
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

/**
 * Service object containing methods for managing surgery appointments
 */
const appointmentService = {
    /**
     * Creates a new surgery appointment
     * @param {Object} appointmentData - The appointment details
     * @returns {Promise<Object>} The created appointment data
     */
    createSurgeryAppointment: async (appointmentData) => {
        const token = getAuthToken();
        try {
            const response = await axios.post(
                `${API_URL}/surgery-appointments`,
                appointmentData,
                {
                    headers: {
                        'Content-Type': 'application/json',
                        'Authorization': `Bearer ${token}`
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.error('Error creating appointment:', error);
            throw error;
        }
    },

    /**
     * Retrieves an appointment by its ID
     * @param {string} appointmentId - ID of the appointment to fetch
     * @returns {Promise<Object>} The appointment data
     */
    getAppointmentById: async (appointmentId) => {
        const token = getAuthToken();
        try {
            const response = await axios.get(
                `${API_URL}/surgery-appointments/${appointmentId}`,
                {
                    headers: {
                        'Authorization': `Bearer ${token}`
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.error('Error fetching appointment:', error);
            throw error;
        }
    },

    /**
     * Updates a surgery appointment by operation request ID
     * @param {string} operationRequestId - ID of the operation request
     * @param {Object} updateData - New appointment data
     * @returns {Promise<Object>} The updated appointment data
     */
    updateSurgeryAppointment: async (operationRequestId, updateData) => {
        const token = getAuthToken();
        try {
            const response = await axios.patch(
                `${API_URL}/surgery-appointments/operation/${operationRequestId}`,
                updateData,
                {
                    headers: {
                        'Content-Type': 'application/json',
                        'Authorization': `Bearer ${token}`
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.error('Error updating appointment:', error);
            throw error;
        }
    },

    /**
     * Gets all appointments for a specific doctor
     * @param {string} doctorId - ID of the doctor
     * @returns {Promise<Array>} Array of appointments
     */
    getDoctorAppointments: async (doctorId) => {
        const token = getAuthToken();
        try {
            const response = await axios.get(
                `${API_URL}/surgery-appointments/doctor/${doctorId}`,
                {
                    headers: {
                        'Authorization': `Bearer ${token}`
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.error('Error fetching doctor appointments:', error);
            throw error;
        }
    },

    /**
     * Searches for appointments based on provided criteria
     * @param {Object} searchParams - Search parameters
     * @returns {Promise<Array>} Array of matching appointments
     */
    searchAppointments: async (searchParams) => {
        const token = getAuthToken();
        try {
            const response = await axios.get(
                `${API_URL}/surgery-appointments/search`,
                {
                    params: searchParams,
                    headers: {
                        'Authorization': `Bearer ${token}`
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.error('Error searching appointments:', error);
            throw error;
        }
    },

    /**
     * Updates the status of an appointment
     * @param {string} appointmentId - ID of the appointment
     * @param {string} status - New status value
     * @returns {Promise<Object>} The updated appointment data
     */
    updateAppointmentStatus: async (appointmentId, status) => {
        const token = getAuthToken();
        try {
            const response = await axios.patch(
                `${API_URL}/surgery-appointments/${appointmentId}/status`,
                { status },
                {
                    headers: {
                        'Authorization': `Bearer ${token}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.error('Error updating appointment status:', error);
            throw error;
        }
    },

    /**
     * Checks if a room is available for a given time period
     * @param {string} roomId - ID of the room to check
     * @param {string} startTime - Start time of the period
     * @param {string} endTime - End time of the period
     * @param {string|null} excludeAppointmentId - Optional appointment ID to exclude from check
     * @returns {Promise<Object>} Room availability status
     */
    checkRoomAvailability: async (roomId, startTime, endTime, excludeAppointmentId = null) => {
        const token = getAuthToken();
        try {
            const response = await axios.get(
                `${API_URL}/surgery-appointments/check-room`,
                {
                    params: {
                        roomId,
                        startTime,
                        endTime,
                        excludeAppointmentId
                    },
                    headers: {
                        'Authorization': `Bearer ${token}`
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.error('Error checking room availability:', error);
            throw error;
        }
    },

    /**
     * Gets an appointment by its operation request ID
     * @param {string} operationRequestId - ID of the operation request
     * @returns {Promise<Object>} The appointment data
     */
    getAppointmentByOperationRequestId: async (operationRequestId) => {
        const token = getAuthToken();
        try {
            const response = await axios.get(
                `${API_URL}/surgery-appointments/operation/${operationRequestId}`,
                {
                    headers: {
                        'Authorization': `Bearer ${token}`
                    }
                }
            );
            return response.data;
        } catch (error) {
            console.error('Error fetching appointment:', error);
            throw error;
        }
    }
};

export default appointmentService;