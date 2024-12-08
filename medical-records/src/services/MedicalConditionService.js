// Author: Matias Vitorino

/**
 * Service layer for handling medical condition business logic.
 * Provides methods for managing and searching medical conditions.
 * Implements validation and business rules for medical condition operations.
 */

const MedicalRecord = require('../models/MedicalRecord');
const MedicalCondition = require('../models/MedicalCondition');
const MedicalConditionDto = require('../dtos/MedicalConditionDto');
const MedicalConditionRepository = require('../repositories/MedicalConditionRepository');

class MedicalConditionService {
    /**
     * Adds a medical condition to a patient's medical record
     * @param {MedicalConditionDto} medicalConditionDto - The medical condition data
     * @returns {Promise<Object>} The created medical condition
     * @throws {Error} If medical record is not found
     */
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

            return await MedicalConditionRepository.addMedicalCondition(newMedicalCondition);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Adds a new medical condition to the system catalog
     * @param {MedicalConditionDto} medicalConditionDto - The medical condition data
     * @returns {Promise<Object>} The created medical condition
     * @throws {Error} If condition already exists
     */
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

            return await MedicalConditionRepository.addMedicalConditionModel(newMedicalCondition);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Searches for medical conditions based on provided filters
     * @param {SearchMedicalConditionDto} searchDto - The search criteria
     * @returns {Promise<Array>} Array of matching medical conditions
     */
    async searchMedicalConditions(searchDto) {
        try {
            console.log('Recebido no service:', searchDto);
            const conditions = await MedicalConditionRepository.findByFilters(searchDto);
            // Retornar as condições diretamente
            return conditions;
        } catch (error) {
            console.error('Erro na busca:', error);
            throw error;
        }
    }


    async getAllConditions() {
        try {
            const conditions = await MedicalConditionRepository.getAllConditionsWithDetails();
            if (!conditions || conditions.length === 0) {
                return [];
            }
            return conditions;
        } catch (error) {
            throw error;
        }
    }
}

module.exports = new MedicalConditionService();