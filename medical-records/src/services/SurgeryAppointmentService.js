// Author: João Morais

/**
 * Service layer for managing surgery appointments.
 * Handles operations for scheduling, updating and searching surgery appointments
 * including validation of operation requests and status management.
 */

const surgeryAppointmentRepository = require('../repositories/SurgeryAppointmentRepository');

class SurgeryAppointmentService {
    /**
     * Creates a new surgery appointment
     * @param {Object} appointmentDto - DTO containing appointment details
     * @returns {Promise<Object>} The newly created appointment
     */
    static async createSurgeryAppointment(appointmentDto) {
        try {
            const appointmentData = {
                operationRequestId: appointmentDto.operationRequestId,
                surgeryRoomId: appointmentDto.surgeryRoomId,
                scheduledDateTime: appointmentDto.scheduledDateTime,
                estimatedDuration: appointmentDto.estimatedDuration,
                staffAssignments: appointmentDto.staffAssignments,
                description: appointmentDto.description,
                status: 'SCHEDULED'
            };

            return await surgeryAppointmentRepository.create(appointmentData);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Retrieves all appointments for a specific doctor
     * @param {string} doctorId - The ID of the doctor
     * @returns {Promise<Array>} Array of appointments
     */
    static async getDoctorAppointments(doctorId) {
        try {
            return await surgeryAppointmentRepository.findByDoctorId(doctorId);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Updates the status of an appointment
     * @param {string} appointmentId - The ID of the appointment
     * @param {string} status - New status to set
     * @returns {Promise<Object>} Updated appointment
     * @throws {Error} If status is invalid or appointment not found
     */
    static async updateAppointmentStatus(appointmentId, status) {
        try {
            const validStatuses = ['SCHEDULED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED'];
            if (!validStatuses.includes(status)) {
                throw new Error('Invalid status');
            }

            const appointment = await surgeryAppointmentRepository.findById(appointmentId);
            if (!appointment) {
                throw new Error('Appointment not found');
            }

            return await surgeryAppointmentRepository.updateStatus(appointmentId, status);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Searches for appointments based on search criteria
     * @param {Object} searchDto - DTO containing search parameters
     * @returns {Promise<Array>} Array of matching appointments
     */
    static async searchAppointments(searchDto) {
        try {
            return await surgeryAppointmentRepository.search(searchDto);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Validates if an operation request matches an appointment
     * @param {string} appointmentId - The ID of the appointment
     * @param {string} operationRequestId - The ID of the operation request
     * @returns {Promise<Object>} The validated appointment
     * @throws {Error} If appointment not found or operation request ID doesn't match
     */
    static async validateOperationRequest(appointmentId, operationRequestId) {
        try {
            const appointment = await surgeryAppointmentRepository.findById(appointmentId);
            
            if (!appointment) {
                throw new Error('Appointment not found');
            }

            if (appointment.operationRequestId !== operationRequestId) {
                throw new Error('Invalid operation request ID');
            }

            return appointment;
        } catch (error) {
            throw error;
        }
    }

    /**
     * Updates an existing surgery appointment
     * @param {string} operationRequestId - The ID of the operation request
     * @param {Object} updateData - Data to update
     * @returns {Promise<Object>} Updated appointment
     */
    static async updateSurgeryAppointment(operationRequestId, updateData) {
        try {            
            return await surgeryAppointmentRepository.update(operationRequestId, updateData);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Finds an appointment by operation request ID
     * @param {string} operationRequestId - The ID of the operation request
     * @returns {Promise<Object>} The matching appointment or null if not found
     */
    static async findByOperationRequestId(operationRequestId) {
        try {
            return await surgeryAppointmentRepository.findByOperationRequestId(operationRequestId);
        } catch (error) {
            throw error;
        }
    }
}

module.exports = SurgeryAppointmentService;