// routes/allergyRoutes.js
const express = require('express');
const router = express.Router();
const Allergy = require('../models/Allergy');
const AllergyController = require('../controllers/AllergyController');

router.post('/add-allergy', AllergyController.addAllergyModel);

router.get('/search', AllergyController.searchAllergies);
router.get('/getAllergyDetails', AllergyController.getAllergyDetails);

router.put('/update-allergy/:id', AllergyController.updateAllergy);
router.delete('/delete-allergy/:id', AllergyController.deleteAllergy);


module.exports = router;