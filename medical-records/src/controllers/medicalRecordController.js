const MedicalRecord = require('../models/MedicalRecord');

exports.createMedicalRecord = async (req, res) => {
    try {
        const medicalRecord = new MedicalRecord(req.body);
        await medicalRecord.save();
        res.status(201).json(medicalRecord);
    } catch (error) {
        res.status(400).json({ error: error.message });
    }
};

exports.getAllMedicalRecords = async (req, res) => {
    try {
        const records = await MedicalRecord.find();
        res.json(records);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
};

exports.getMedicalRecordByPatientId = async (req, res) => {
    try {
        const record = await MedicalRecord.findOne({ patientId: req.params.patientId });
        if (!record) {
            return res.status(404).json({ message: 'Medical record not found' });
        }
        res.json(record);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
};

exports.updateMedicalRecord = async (req, res) => {
    try {
        const record = await MedicalRecord.findByIdAndUpdate(
            req.params.id,
            req.body,
            { new: true }
        );
        if (!record) {
            return res.status(404).json({ message: 'Medical record not found' });
        }
        res.json(record);
    } catch (error) {
        res.status(400).json({ error: error.message });
    }
};