const MedicalRecord = require('../models/MedicalRecord');
const AlergyRepository = require('../repositories/AllergyRepositorie');


class AllergyService {
    async addAllergy(patientId, allergyDto) {
        try {
            const medicalRecord = await MedicalRecord.findOne({ patientId });
            console.log(medicalRecord);
            
            if (!medicalRecord) {
                throw new Error('Medical record not found');
            }

            const newAllergy = {
                allergen: allergyDto.allergen,
                severity: allergyDto.severity,
                diagnosedDate: allergyDto.diagnosedDate,
                notes: allergyDto.notes
            };

            const allergy = await AlergyRepository.addAllergy(patientId, newAllergy);        
            
            return allergy;
        } catch (error) {
            throw error;
        }
    }

    async getAllergies(patientId) {
        try {
            const medicalRecord = await MedicalRecord.findOne({ patientId });
            
            if (!medicalRecord) {
                throw new Error('Medical record not found');
            }

            return medicalRecord.allergies;
        } catch (error) {
            throw error;
        }
    }
}

module.exports = new AllergyService();
