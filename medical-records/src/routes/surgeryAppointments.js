// Author: Pedro Azevedo

/**
 * This module defines the routes for surgery appointment operations
 * Includes endpoints for creating, reading, updating and searching surgery appointments
 * Handles routing of HTTP requests to the appropriate controller methods
 */

const express = require('express');
const router = express.Router();
const SurgeryAppointmentController = require('../controllers/SurgeryAppointmentController');

/**
 * POST /
 * Creates a new surgery appointment in the system
 * Accepts appointment data in request body
 */
router.post('/', SurgeryAppointmentController.createSurgeryAppointment);

/**
 * GET /doctor/:doctorId
 * Retrieves all surgery appointments for a specific doctor
 * @param {string} doctorId - The ID of the doctor to get appointments for
 */
router.get('/doctor/:doctorId', SurgeryAppointmentController.getDoctorAppointments);

/**
 * GET /search
 * Searches for appointments based on provided filters
 * Accepts search criteria as query parameters
 */
router.get('/search', SurgeryAppointmentController.searchAppointments);

/**
 * PATCH /:appointmentId/status
 * Updates the status of a specific appointment
 * @param {string} appointmentId - The ID of the appointment to update
 */
router.patch('/:appointmentId/status', SurgeryAppointmentController.updateAppointmentStatus);

/**
 * PATCH /:operationRequestId/update
 * Updates details of a surgery appointment by operation request ID
 * @param {string} operationRequestId - The ID of the operation request
 */
router.patch('/:operationRequestId/update', SurgeryAppointmentController.updateSurgeryAppointment);

/**
 * GET /operation/:operationRequestId
 * Retrieves a specific appointment by operation request ID
 * @param {string} operationRequestId - The ID of the operation request
 */
router.get('/operation/:operationRequestId', SurgeryAppointmentController.getByOperationRequestId);

/**
 * PATCH /operation/:operationRequestId
 * Updates a surgery appointment by operation request ID
 * @param {string} operationRequestId - The ID of the operation request
 */
router.patch('/operation/:operationRequestId', SurgeryAppointmentController.updateSurgeryAppointment);

module.exports = router;