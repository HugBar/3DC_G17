// Author: Matias Vitorino

/**
 * This module defines the routes for medical condition operations
 * Includes endpoints for adding and searching medical conditions
 */
const express = require('express');
const router = express.Router();
const MedicalConditionController = require('../controllers/medicalConditionController');

// Define routes for medical condition operations
router.post('/add-medical-condition', MedicalConditionController.addMedicalConditionModel);
router.get('/search', MedicalConditionController.searchMedicalConditions);
router.get('/getConditionDetails', MedicalConditionController.getConditionDetails);
router.put('/update/:id', MedicalConditionController.updateMedicalCondition);
router.get('/:id', MedicalConditionController.getMedicalCondition);

module.exports = router;