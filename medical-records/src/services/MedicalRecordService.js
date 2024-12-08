// Author: Joao Morais

/**
 * Service layer for medical record business logic.
 * Handles operations for managing patient medical records including:
 * - Updating conditions and allergies
 * - Creating new records when needed
 * - Data transformation and validation
 */

const MedicalRecord = require('../models/MedicalRecord');
const MedicalRecordRepository = require('../repositories/MedicalRecordRepository');

class MedicalRecordService {
    /**
     * Updates or creates a patient's medical record with conditions and allergies
     * @param {string} patientId - The ID of the patient
     * @param {Object} updateDto - DTO containing conditions and allergies to update
     * @returns {Promise<Object>} The updated medical record
     */
    async updatePatientConditionsAndAllergies(patientId, updateDto) {
        try {
            let medicalRecord = await MedicalRecordRepository.findByPatientId(patientId);
            
            if (!medicalRecord) {
                // Create new medical record if it doesn't exist
                const newRecordData = {
                    patientId,
                    conditions: [],
                    allergies: []
                };
                medicalRecord = await MedicalRecordRepository.create(newRecordData);
            }

            // Transform conditions and allergies data for update
            const updateData = {
                conditions: updateDto.conditions.map(condition => ({
                    name: condition.name,
                    severity: condition.severity
                })),
                allergies: updateDto.allergies.map(allergy => ({
                    name: allergy.name,
                    severity: allergy.severity
                })),
                lastUpdated: new Date()
            };

            // Persist the updated record using repository
            const updatedRecord = await MedicalRecordRepository.update(patientId, updateData);
            return updatedRecord;
        } catch (error) {
            throw error;
        }
    }
}

module.exports = new MedicalRecordService();