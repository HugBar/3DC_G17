const MedicalRecord = require('../models/MedicalRecord');

exports.findByPatientId = async (patientId) => {
    return await MedicalRecord.findOne({ patientId });
};

// Add new allergy to patient's medical record
exports.addAllergy = async (patientId, allergyDto) => {
    try {
        const medicalRecord = await MedicalRecord.findOne({ patientId });
        
        if (!medicalRecord) {
            throw new Error('Medical record not found');
        }

        medicalRecord.allergies.push(allergyDto);
        await medicalRecord.save();

        return allergyDto;
    }catch (error) {
        throw error;
    }
};