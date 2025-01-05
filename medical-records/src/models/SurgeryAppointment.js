// Author: João Morais

/**
 * This module defines the Mongoose schema and model for Surgery Appointments.
 * It represents scheduled surgical procedures in the system with fields for:
 * - Operation request ID
 * - Surgery room allocation
 * - Scheduled date/time
 * - Duration estimate
 * - Staff assignments (doctors, nurses, technicians)
 * - Optional description
 */

const mongoose = require('mongoose');

/**
 * Mongoose schema definition for Surgery Appointment
 * Includes validation rules and relationships for:
 * - operationRequestId: References the surgery request
 * - surgeryRoomId: References the assigned operating room
 * - scheduledDateTime: When the surgery is scheduled for
 * - estimatedDuration: Expected length in minutes
 * - staffAssignments: Array of assigned medical staff with roles
 * - description: Optional notes about the surgery
 * 
 * Methods available through the model:
 * - create(): Creates a new surgery appointment
 * - find(): Retrieves appointments matching criteria
 * - findOne(): Finds a single appointment
 * - findById(): Retrieves an appointment by ID
 * - updateOne(): Updates a single appointment
 * - deleteOne(): Removes a single appointment
 */
const surgeryAppointmentSchema = new mongoose.Schema({
    operationRequestId: {
        type: String,
        required: true
    },
    surgeryRoomId: {
        type: String,
        required: true
    },
    scheduledDateTime: {
        type: Date,
        required: true
    },
    estimatedDuration: {
        type: Number,
        required: true
    },
    staffAssignments: [{
        licenseNumber: {
            type: String,
            required: true,
            match: /^LIC-\d{8}$/ // Validates license number format
        },
        role: {
            type: String,
            required: true,
            enum: ['DOCTOR', 'NURSE','TECHNICIAN'] // Restricts to valid roles
        }
    }],
    description: String
});

/**
 * Creates and exports the SurgeryAppointment model
 * This model provides an interface for performing CRUD operations on surgery appointments
 * Inherits all Mongoose model methods for database interactions
 * @returns {Model} Mongoose model for SurgeryAppointment documents
 */
module.exports = mongoose.model('SurgeryAppointment', surgeryAppointmentSchema);