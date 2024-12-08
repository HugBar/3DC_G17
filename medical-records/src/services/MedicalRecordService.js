const MedicalRecord = require('../models/MedicalRecord');

class MedicalRecordService {
    async updatePatientConditionsAndAllergies(patientId, updateDto) {
        try {
            let medicalRecord = await MedicalRecord.findOne({ patientId });
            
            if (!medicalRecord) {
                // Create new medical record if it doesn't exist
                medicalRecord = new MedicalRecord({
                    patientId,
                    conditions: [],
                    allergies: []
                });
            }

            // Update the conditions and allergies
            medicalRecord.conditions = updateDto.conditions.map(condition => ({
                name: condition.name,
                severity: condition.severity
            }));

            medicalRecord.allergies = updateDto.allergies.map(allergy => ({
                name: allergy.name,
                severity: allergy.severity
            }));

            medicalRecord.lastUpdated = new Date();
            
            await medicalRecord.save();
            return medicalRecord;
        } catch (error) {
            throw error;
        }
    }
}

module.exports = new MedicalRecordService(); 