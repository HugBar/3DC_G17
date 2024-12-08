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

            let filteredRecord = {
                _id: record._id,
                patientId: record.patientId,
                lastUpdated: record.lastUpdated,
                createdAt: record.createdAt,
                updatedAt: record.updatedAt
            };

            // Only include conditions if searching for conditions or no specific filter
            if (searchDto.conditionName || (!searchDto.conditionName && !searchDto.allergyName)) {
                filteredRecord.conditions = record.conditions.filter(condition =>
                    !searchDto.conditionName || 
                    condition.name.toLowerCase().includes(searchDto.conditionName.toLowerCase())
                );
            }

            // Only include allergies if searching for allergies or no specific filter
            if (searchDto.allergyName || (!searchDto.conditionName && !searchDto.allergyName)) {
                filteredRecord.allergies = record.allergies.filter(allergy =>
                    !searchDto.allergyName || 
                    allergy.name.toLowerCase().includes(searchDto.allergyName.toLowerCase())
                );
            }

            return filteredRecord;
        } catch (error) {
            throw new Error(`Error searching medical record: ${error.message}`);
        }
    }
}

module.exports = new MedicalRecordService();