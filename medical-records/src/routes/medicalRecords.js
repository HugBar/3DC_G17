// Author: João Morais

/**
 * This module defines the routes for medical record operations
 * Includes endpoints for creating, reading, updating and searching medical records
 * Handles routing of HTTP requests to the appropriate controller methods
 */

const express = require('express');
const router = express.Router();
const medicalRecordController = require('../controllers/medicalRecordController');

/**
 * POST /create/:patientId
 * Creates a new blank medical record for a patient
 * Accepts patient ID as URL parameter
 */
router.post('/create/:patientId', medicalRecordController.createBlankMedicalRecord);

/**
 * GET /
 * Retrieves all medical records in the system
 * Returns sanitized medical record data
 */
router.get('/', medicalRecordController.getAllMedicalRecords);

/**
 * GET /search
 * Searches for medical records based on provided filters
 * Accepts search criteria as query parameters
 */
router.get('/search', medicalRecordController.searchMedicalRecord);

/**
 * GET /:patientId
 * Retrieves a specific medical record by patient ID
 * Returns the medical record details if found
 */
router.get('/:patientId', medicalRecordController.getMedicalRecordByPatientId);

/**
 * PUT /update/:patientId
 * Updates conditions and allergies for a patient's medical record
 * Accepts updated medical data in request body
 */
router.put('/update/:patientId', medicalRecordController.updatePatientConditionsAndAllergies);

module.exports = router;
