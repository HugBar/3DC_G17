const express = require('express');
const router = express.Router();
const { isAdmin } = require('../middleware/auth');
const SpecializationController = require('../controllers/SpecializationController');

// Add new specialization (Admin only)
router.post('/', SpecializationController.addSpecialization);

// Get all specializations
router.get('/', SpecializationController.getAllSpecializations);

// Get specialization by ID
router.get('/:id', SpecializationController.getSpecializationById);

module.exports = router; 