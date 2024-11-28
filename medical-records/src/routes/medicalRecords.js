const express = require('express');
const router = express.Router();
const MedicalRecord = require('../models/MedicalRecord');

// Create a medical record
router.post('/', async (req, res) => {
    try {
        const medicalRecord = new MedicalRecord(req.body);
        await medicalRecord.save();
        res.status(201).json(medicalRecord);
    } catch (error) {
        res.status(400).json({ error: error.message });
    }
});

// Get all medical records
router.get('/', async (req, res) => {
    try {
        const records = await MedicalRecord.find();
        res.json(records);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

module.exports = router;