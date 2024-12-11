// Author: [Your Name]

/**
 * This module provides API endpoints for managing surgery appointments.
 * It handles the creation, retrieval, and management of surgery appointments.
 * Only authorized doctors can create and manage appointments.
 */

const SurgeryAppointmentService = require('../services/SurgeryAppointmentService');
const SurgeryAppointmentDto = require('../dtos/SurgeryAppointmentDto');

class SurgeryAppointmentController {
    static async createSurgeryAppointment(req, res) {
        try {
            const appointmentDto = new SurgeryAppointmentDto(req.body);
            
            const result = await SurgeryAppointmentService.createSurgeryAppointment(appointmentDto);
            
            res.status(201).json({
                message: 'Surgery appointment created successfully',
                appointment: result
            });
        } catch (error) {
            console.error('Error creating surgery appointment:', error);
            if (error.message.startsWith('Missing required field:') || 
                error.message.startsWith('Invalid role:') ||
                error.message === 'Staff assignments must be a non-empty array' ||
                error.message === 'Each staff assignment must have staffId and role' ||
                error.message === 'Invalid scheduledDateTime') {
                res.status(400).json({ message: error.message });
            } else if (error.message === 'Room not available') {
                res.status(409).json({ message: error.message });
            } else {
                res.status(500).json({ message: 'Internal server error' });
            }
        }
    }

    static async getDoctorAppointments(req, res) {
        try {
            const { doctorId } = req.params;
            const appointments = await SurgeryAppointmentService.getDoctorAppointments(doctorId);
            res.status(200).json(appointments);
        } catch (error) {
            console.error('Error getting appointments:', error);
            res.status(500).json({ message: 'Internal server error' });
        }
    }

    static async updateAppointmentStatus(req, res) {
        try {
            const { appointmentId } = req.params;
            const { status } = req.body;
            
            const result = await SurgeryAppointmentService.updateAppointmentStatus(appointmentId, status);
            
            res.status(200).json({
                message: 'Appointment status updated successfully',
                appointment: result
            });
        } catch (error) {
            console.error('Error updating appointment status:', error);
            if (error.message === 'Appointment not found') {
                res.status(404).json({ message: error.message });
            } else if (error.message === 'Invalid status') {
                res.status(400).json({ message: error.message });
            } else {
                res.status(500).json({ message: 'Internal server error' });
            }
        }
    }

    static async searchAppointments(req, res) {
        try {
            const searchDto = {
                doctorId: req.query.doctorId,
                patientId: req.query.patientId,
                startDate: req.query.startDate,
                endDate: req.query.endDate,
                status: req.query.status
            };

            const appointments = await SurgeryAppointmentService.searchAppointments(searchDto);
            res.status(200).json(appointments);
        } catch (error) {
            console.error('Error searching appointments:', error);
            res.status(500).json({ message: 'Internal server error' });
        }
    }
}

module.exports = SurgeryAppointmentController;