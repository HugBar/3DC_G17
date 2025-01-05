// Author: Hugo Barros

/**
 * This module defines the routes for allergy-related operations
 * Includes endpoints for creating, reading, updating and deleting allergy records
 * Handles routing of HTTP requests to the appropriate controller methods
 */

const express = require('express');
const router = express.Router();
const Allergy = require('../models/Allergy');
const AllergyController = require('../controllers/AllergyController');

/**
 * POST /add-allergy
 * Creates a new allergy record in the system
 * Accepts allergy data in request body
 */
router.post('/add-allergy', AllergyController.addAllergyModel);

/**
 * GET /search
 * Searches for allergies based on provided filters
 * Accepts search criteria as query parameters
 */
router.get('/search', AllergyController.searchAllergies);

/**
 * GET /getAllergyDetails
 * Retrieves detailed information about all allergies
 * Returns sanitized allergy records
 */
router.get('/getAllergyDetails', AllergyController.getAllergyDetails);

/**
 * PUT /update-allergy/:id
 * Updates an existing allergy record by ID
 * Accepts updated allergy data in request body
 */
router.put('/update-allergy/:id', AllergyController.updateAllergy);

/**
 * DELETE /delete-allergy/:id 
 * Deletes an allergy record by ID
 * Removes the allergy from the system
 */
router.delete('/delete-allergy/:id', AllergyController.deleteAllergy);

module.exports = router;