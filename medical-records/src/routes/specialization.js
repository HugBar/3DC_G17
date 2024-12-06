// Author: João Morais

/**
 * This module defines the routes for managing medical specializations.
 * It provides endpoints for:
 * - Creating new specializations (admin only)
 * - Retrieving all specializations
 * - Fetching specific specializations by ID
 */

const express = require('express');
const router = express.Router();
const { isAdmin } = require('../middleware/auth');
const SpecializationController = require('../controllers/SpecializationController');


router.get('/search', SpecializationController.searchSpecialization);

/**
 * POST /
 * Creates a new specialization
 * Restricted to admin users only
 */
router.post('/', SpecializationController.addSpecialization);

/**
 * GET /
 * Retrieves all specializations
 * Accessible to all authenticated users
 */
router.get('/', SpecializationController.getAllSpecializations);

/**
 * GET /:id
 * Retrieves a specific specialization by ID
 * Accessible to all authenticated users
 * @param {string} id - The ID of the specialization to retrieve
 */
router.get('/:id', SpecializationController.getSpecializationById);


module.exports = router;