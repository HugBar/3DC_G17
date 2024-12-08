const express = require('express');
const router = express.Router();
const medicalRecordController = require('../controllers/medicalRecordController');

// Existing routes
router.get('/', medicalRecordController.getAllMedicalRecords);
router.get('/:patientId', medicalRecordController.getMedicalRecordByPatientId);

// Add the new update route
router.put('/update/:patientId', medicalRecordController.updatePatientConditionsAndAllergies);

module.exports = router;
