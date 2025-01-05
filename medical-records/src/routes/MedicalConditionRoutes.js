// Author: Matias Vitorino

/**
 * This module defines the routes for medical condition-related operations
 * Includes endpoints for creating, reading, updating and searching medical conditions
 * Handles routing of HTTP requests to the appropriate controller methods
 */

const express = require('express');
const router = express.Router();
const MedicalConditionController = require('../controllers/medicalConditionController');

/**
 * POST /add-medical-condition
 * Creates a new medical condition record in the system
 * Accepts medical condition data in request body
 */
router.post('/add-medical-condition', MedicalConditionController.addMedicalConditionModel);

/**
 * GET /search
 * Searches for medical conditions based on provided filters
 * Accepts search criteria as query parameters
 */
router.get('/search', MedicalConditionController.searchMedicalConditions);

/**
 * GET /getConditionDetails
 * Retrieves detailed information about all medical conditions
 * Returns sanitized medical condition records
 */
router.get('/getConditionDetails', MedicalConditionController.getConditionDetails);

/**
 * PUT /update/:id
 * Updates an existing medical condition record by ID
 * Accepts updated medical condition data in request body
 */
router.put('/update/:id', MedicalConditionController.updateMedicalCondition);

/**
 * GET /:id
 * Retrieves a specific medical condition by its ID
 * Returns the medical condition details if found
 */
router.get('/:id', MedicalConditionController.getMedicalCondition);

module.exports = router;