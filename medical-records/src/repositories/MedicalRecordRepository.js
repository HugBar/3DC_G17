// Author: Joao Morais

/**
 * Repository layer for medical record data operations.
 * Handles all database interactions for medical records including:
 * - Creating new records
 * - Finding records by patient ID
 * - Updating records
 * - Retrieving all records
 */

const MedicalRecord = require('../models/MedicalRecord');

const sanitizeMedicalRecord = (record) => ({
    _id: record._id,
    patientId: record.patientId,
    conditions: record.conditions,
    allergies: record.allergies,
    createdAt: record.createdAt,
    updatedAt: record.updatedAt
});

/**
 * Creates a new medical record
 * @param {Object} medicalRecordData - The medical record data
 * @returns {Promise<Object>} The created medical record
 */
exports.create = async (medicalRecordData) => {
    return await MedicalRecord.create(medicalRecordData);
};

/**
 * Finds a medical record by patient ID
 * @param {string} patientId - The ID of the patient
 * @returns {Promise<Object>} The found medical record
 */
exports.findByPatientId = async (patientId) => {
    return await MedicalRecord.findOne({ patientId });
};

/**
 * Retrieves all medical records
 * @returns {Promise<Array>} Array of all medical records
 */
exports.findAll = async () => {
    const records = await MedicalRecord.find();
    return records.map(record => sanitizeMedicalRecord(record));
};

/**
 * Updates a medical record
 * @param {string} patientId - The ID of the patient
 * @param {Object} updateData - The update data
 * @returns {Promise<Object>} The updated medical record
 */
exports.update = async (patientId, updateData) => {
    const record = await MedicalRecord.findOneAndUpdate(
        { patientId },
        updateData,
        { new: true }
    );
    return record ? sanitizeMedicalRecord(record) : null;
}; 