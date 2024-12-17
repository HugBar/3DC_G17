const express = require('express');
const router = express.Router();
const SurgeryAppointmentController = require('../controllers/SurgeryAppointmentController');

// Create new surgery appointment
router.post('/', SurgeryAppointmentController.createSurgeryAppointment);

// Get doctor's appointments
router.get('/doctor/:doctorId', SurgeryAppointmentController.getDoctorAppointments);

// Search appointments
router.get('/search', SurgeryAppointmentController.searchAppointments);

// Update appointment status
router.patch('/:appointmentId/status', SurgeryAppointmentController.updateAppointmentStatus);

module.exports = router;