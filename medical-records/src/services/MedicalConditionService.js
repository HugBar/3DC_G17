const MedicalRecord = require('../models/MedicalRecord');
const MedicalCondition = require('../models/MedicalCondition');
const MedicalConditionDto = require('../dtos/MedicalConditionDto');
const MedicalConditionRepository = require('../repositories/MedicalConditionRepository');

class MedicalConditionService {
    async addMedicalCondition(medicalConditionDto) {
        try {
            const medicalRecord = await MedicalRecord.findOne({ patientId });
            console.log(medicalRecord);
            
            if (!medicalRecord) {
                throw new Error('Medical record not found');
            }

            const newMedicalCondition = {
                name: medicalConditionDto.name,
                severity: medicalConditionDto.severity,
                description: medicalConditionDto.description
            };

            const medicalCondition = await MedicalConditionRepository.addMedicalCondition(newMedicalCondition);        
            
            return medicalCondition;
        } catch (error) {
            throw error;
        }
    }

    async addMedicalConditionModel(medicalConditionDto) {
        try {
            const medicalCondition = await MedicalCondition.findOne({ name: medicalConditionDto.name });
            if (medicalCondition) {
                throw new Error('Medical condition already exists');
            }

            const newMedicalCondition = new MedicalCondition({
                name: medicalConditionDto.name,
                severity: medicalConditionDto.severity,
                description: medicalConditionDto.description
            });

            const addedCondition = await MedicalConditionRepository.addMedicalConditionModel(newMedicalCondition);
            return addedCondition;

        } catch (error) {
            throw error;
        }
    }

    async searchMedicalConditions(searchDto) {
        try {
            const conditions = await MedicalConditionRepository.findByFilters(searchDto);
            return conditions.map(condition => new MedicalConditionDto(condition));
        } catch (error) {
            throw error;
        }
    }
}

module.exports = new MedicalConditionService();