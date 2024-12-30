import axios from 'axios';

const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001/api';

const getAuthToken = () => {
    const token = localStorage.getItem('authToken');
    if (!token) {
        throw new Error('No auth token found');
    }
    return token;
};

const appointmentService = {
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

    updateSurgeryAppointment: async (appointmentId, updateData) => {
        const token = getAuthToken();
        try {
            const updateResponse = await axios.patch(
                `${API_URL}/surgery-appointments/${appointmentId}`,
                updateData,
                {
                    headers: {
                        'Content-Type': 'application/json',
                        'Authorization': `Bearer ${token}`
                    }
                }
            );
            return updateResponse.data;
        } catch (error) {
            console.error('Error updating appointment:', error);
            throw error;
        }
    },

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
    }
};

export default appointmentService;