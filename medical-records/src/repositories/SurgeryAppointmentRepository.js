// Author: Pedro Azevedo

/**
 * Repository layer for surgery appointment data operations.
 * Handles all database interactions for surgery appointments including:
 * - Creating new appointments
 * - Finding appointments by various criteria
 * - Checking room availability
 * - Updating appointment status and details
 * - Searching appointments with filters
 */

const SurgeryAppointment = require('../models/SurgeryAppointment');

class SurgeryAppointmentRepository {
    constructor() {
        if (SurgeryAppointmentRepository.instance) {
            return SurgeryAppointmentRepository.instance;
        }
        SurgeryAppointmentRepository.instance = this;
    }

    /**
     * Creates a new surgery appointment
     * @param {Object} appointmentData - The appointment data to create
     * @returns {Promise<Object>} The created appointment
     */
    async create(appointmentData) {
        try {
            const appointment = new SurgeryAppointment(appointmentData);
            return await appointment.save();
        } catch (error) {
            throw error;
        }
    }

    /**
     * Finds an appointment by its ID
     * @param {string} id - The ID of the appointment
     * @returns {Promise<Object>} The found appointment
     */
    async findById(id) {
        try {
            return await SurgeryAppointment.findById(id);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Finds all appointments for a specific doctor
     * @param {string} doctorId - The ID of the doctor
     * @returns {Promise<Array>} Array of appointments
     */
    async findByDoctorId(doctorId) {
        try {
            return await SurgeryAppointment.find({ doctorId });
        } catch (error) {
            throw error;
        }
    }

    /**
     * Checks if a surgery room is available for a given time slot
     * @param {string} roomId - The ID of the surgery room
     * @param {Date} startTime - The start time of the slot
     * @param {Date} endTime - The end time of the slot
     * @param {string} excludeAppointmentId - Optional ID of appointment to exclude from check
     * @returns {Promise<boolean>} True if room is available
     */
    async checkRoomAvailability(roomId, startTime, endTime, excludeAppointmentId = null) {
        try {
            const query = {
                surgeryRoomId: roomId,
                status: { $ne: 'CANCELLED' },
                $or: [
                    {
                        scheduledDateTime: { $lt: endTime },
                        endDateTime: { $gt: startTime }
                    }
                ]
            };

            if (excludeAppointmentId) {
                query._id = { $ne: excludeAppointmentId };
            }

            const conflictingAppointments = await SurgeryAppointment.find(query);
            return conflictingAppointments.length === 0;
        } catch (error) {
            throw error;
        }
    }

    /**
     * Updates the status of an appointment
     * @param {string} id - The ID of the appointment
     * @param {string} status - The new status
     * @returns {Promise<Object>} The updated appointment
     */
    async updateStatus(id, status) {
        try {
            return await SurgeryAppointment.findByIdAndUpdate(
                id,
                { status },
                { new: true }
            );
        } catch (error) {
            throw error;
        }
    }

    /**
     * Searches appointments using provided filters
     * @param {Object} filters - Search criteria including doctorId, patientId, status, startDate, endDate
     * @returns {Promise<Array>} Array of matching appointments
     */
    async search(filters) {
        try {
            const query = {};
            
            if (filters.doctorId) query.doctorId = filters.doctorId;
            if (filters.patientId) query.patientId = filters.patientId;
            if (filters.status) query.status = filters.status;
            if (filters.startDate || filters.endDate) {
                query.scheduledDateTime = {};
                if (filters.startDate) query.scheduledDateTime.$gte = new Date(filters.startDate);
                if (filters.endDate) query.scheduledDateTime.$lte = new Date(filters.endDate);
            }

            return await SurgeryAppointment.find(query);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Updates an appointment by ID
     * @param {string} id - The ID of the appointment
     * @param {Object} updateData - The data to update
     * @returns {Promise<Object>} The updated appointment
     */
    async update(id, updateData) {
        try {
            return await SurgeryAppointment.findByIdAndUpdate(
                id,
                updateData,
                { new: true, runValidators: true }
            );
        } catch (error) {
            throw error;
        }
    }

    /**
     * Finds an appointment by operation request ID
     * @param {string} operationRequestId - The ID of the operation request
     * @returns {Promise<Object>} The found appointment
     */
    async findByOperationRequestId(operationRequestId) {
        try {
            return await SurgeryAppointment.findOne({ operationRequestId });
        } catch (error) {
            throw error;
        }
    }

    /**
     * Updates an appointment by operation request ID
     * @param {string} operationRequestId - The ID of the operation request
     * @param {Object} updateData - The data to update
     * @returns {Promise<Object>} The updated appointment
     */
    async update(operationRequestId, updateData) {
        try {
            const appointment = await SurgeryAppointment.findOne({ operationRequestId });
            
            if (!appointment) {
                throw new Error('Appointment not found');
            }

            // Update the appointment with the new data
            Object.assign(appointment, updateData);
            
            // Save the updated appointment
            return await appointment.save();
        } catch (error) {
            throw error;
        }
    }
}

const surgeryAppointmentRepository = new SurgeryAppointmentRepository();
module.exports = surgeryAppointmentRepository;