// Author: [Pedro Azevedo]

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
            const appointmentData = req.body;
            
            // Validar staff assignments
            if (!appointmentData.staffAssignments || !Array.isArray(appointmentData.staffAssignments)) {
                return res.status(400).json({ message: "Staff assignments are required" });
            }

            for (const staff of appointmentData.staffAssignments) {
                if (!staff.licenseNumber || !staff.role) {
                    return res.status(400).json({ 
                        message: "Each staff assignment must have licenseNumber and role" 
                    });
                }
            }

            const newAppointment = await SurgeryAppointmentService.createSurgeryAppointment(appointmentData);
            res.status(201).json(newAppointment);
        } catch (error) {
            res.status(500).json({ message: error.message });
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

    static async updateSurgeryAppointment(req, res) {
        try {
            const { operationRequestId } = req.params;
            const updateData = req.body;

            const updatedAppointment = await SurgeryAppointmentService.updateSurgeryAppointment(
                operationRequestId, 
                updateData
            );
            
            res.status(200).json(updatedAppointment);
        } catch (error) {
            if (error.message === 'Appointment not found') {
                return res.status(404).json({ message: error.message });
            }
            if (error.message === 'Room not available') {
                return res.status(409).json({ message: error.message });
            }
            res.status(500).json({ message: error.message });
        }
    }
}

module.exports = SurgeryAppointmentController;