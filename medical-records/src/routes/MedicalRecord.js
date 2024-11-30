const express = require('express');
const router = express.Router();
const authMiddleware = require('../middleware/auth');
const AllergyController = require('../controllers/AllergyController');

// Add new allergy to patient's medical record
router.post('/add-allergies/:patientId', AllergyController.addAllergy);

// get 
router.get('/', async (req, res) => {
    try {
        res.json({ message: 'Medical Records Service is running' });
    } catch (error) {
        console.error(error);
        res.status(500).json({ message: 'Internal server error' });
    }
});

// Exporta o router
module.exports = router;