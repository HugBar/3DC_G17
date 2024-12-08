const MedicalRecord = require('../models/MedicalRecord');
const UpdateMedicalRecordDto = require('../dtos/UpdateMedicalRecordDto');
const MedicalRecordService = require('../services/MedicalRecordService');

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

exports.updatePatientConditionsAndAllergies = async (req, res) => {
    try {
        const { patientId } = req.params;
        const { conditions, allergies } = req.body;
        
        const updateDto = new UpdateMedicalRecordDto(conditions, allergies);
        
        const existingRecord = await MedicalRecord.findOne({ patientId });
        const isNewRecord = !existingRecord;
        
        const updatedRecord = await MedicalRecordService.updatePatientConditionsAndAllergies(
            patientId, 
            updateDto
        );

        res.status(isNewRecord ? 201 : 200).json({
            message: isNewRecord ? 
                'Medical record created successfully' : 
                'Medical record updated successfully',
            record: updatedRecord
        });
    } catch (error) {
        console.error('Error updating medical record:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};