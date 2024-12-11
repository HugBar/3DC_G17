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
        // Get existing record
        const medicalRecord = await MedicalRecordRepository.findByPatientId(patientId);
        if (!medicalRecord) {
            throw new Error('Medical record not found');
        }

        // Update the record
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

        return await MedicalRecordRepository.update(patientId, updateData);
    }

    /**
     * Searches for a medical record by patient ID and optionally filters by condition or allergy
     * @param {Object} searchDto - DTO containing search parameters (patientId, conditionName, allergyName)
     * @returns {Promise<Object>} The filtered medical record or null if not found
     */
    async searchMedicalRecord(searchDto) {
        try {
            searchDto.validate();
            const record = await MedicalRecordRepository.findByPatientId(searchDto.patientId);
            
            if (!record) {
                return null;
            }

            const result = {
                _id: record._id,
                patientId: record.patientId,
                conditions: [],
                allergies: [],
                lastUpdated: record.lastUpdated,
                createdAt: record.createdAt,
                updatedAt: record.updatedAt
            };

            // Only include conditions if searching by condition name
            if (searchDto.conditionName) {
                result.conditions = record.conditions.filter(condition => 
                    condition.name.toLowerCase().includes(searchDto.conditionName.toLowerCase())
                );
            } else {
                result.conditions = record.conditions;
            }

            // Only include allergies if searching by allergy name
            if (searchDto.allergyName) {
                result.allergies = record.allergies.filter(allergy => 
                    allergy.name.toLowerCase().includes(searchDto.allergyName.toLowerCase())
                );
            } else if (!searchDto.conditionName) {
                // Include all allergies only if not searching by condition
                result.allergies = record.allergies;
            }

            return result;
        } catch (error) {
            throw new Error(`Error searching medical record: ${error.message}`);
        }
    }
}

module.exports = new MedicalRecordService();