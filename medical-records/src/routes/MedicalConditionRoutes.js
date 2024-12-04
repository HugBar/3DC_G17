const express = require('express');
const router = express.Router();
const MedicalConditionController = require('../controllers/medicalConditionController');

// Remover temporariamente a autenticação
// router.use(authMiddleware.authenticate);

router.post('/add-medical-condition', MedicalConditionController.addMedicalConditionModel);
router.get('/search', MedicalConditionController.searchMedicalConditions);

module.exports = router;