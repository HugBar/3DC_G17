// Author: Matias Vitorino

/**
 * Repository layer for medical condition data operations.
 * Handles all database interactions for medical conditions including:
 * - Finding records by patient ID
 * - Adding new conditions
 * - Searching conditions with filters
 */

const MedicalRecord = require('../models/MedicalRecord');
const MedicalCondition = require('../models/MedicalCondition');

const sanitizeCondition = (condition) => ({
    _id: condition._id,
    name: condition.name,
    severity: condition.severity
});

/**
 * Finds a medical record by patient ID
 * @param {string} patientId - The ID of the patient
 * @returns {Promise<Object>} The found medical record
 */
exports.findByPatientId = async (patientId) => {
    return await MedicalRecord.findOne({ patientId });
};

/**
 * Finds a medical condition by its name
 * @param {string} name - The name of the medical condition
 * @returns {Promise<Object>} The found medical condition
 */
exports.findByName = async (name) => {
    return await MedicalCondition.findOne({ name });
};

/**
 * Adds a new medical condition to a patient's medical record
 * @param {Object} medicalConditionDto - The medical condition data
 * @returns {Promise<Object>} The created medical condition
 */
exports.addMedicalCondition = async (medicalConditionDto) => {
    try {
        const medicalRecord = await MedicalRecord.findOne({ patientId });
        
        if (!medicalRecord) {
            throw new Error('Medical record not found');
        }

        medicalRecord.medicalConditions.push(medicalConditionDto);
        await medicalRecord.save();

        return medicalConditionDto;
    } catch (error) {
        throw error;
    }
};

/**
 * Adds a new medical condition to the system catalog
 * @param {Object} medicalCondition - The medical condition data
 * @returns {Promise<Object>} The created medical condition
 */
exports.addMedicalConditionModel = async (medicalCondition) => {
    return await MedicalCondition.create(medicalCondition);
};

/**
 * Retrieves all medical conditions from the catalog
 * @returns {Promise<Array>} Array of all medical conditions
 */
exports.getAllMedicalConditions = async () => {
    return await MedicalCondition.find();
};

/**
 * Searches medical conditions based on provided filters
 * @param {Object} filters - Search criteria (name, severity)
 * @returns {Promise<Array>} Array of matching medical conditions
 */
exports.findByFilters = async (filters) => {
    const query = {};

    if (filters.name) {
        query.name = new RegExp(filters.name, 'i');
    }
    if (filters.severity) {
        query.severity = filters.severity;
    }
    const results = await MedicalCondition.find(query);

    return results;
};

exports.getAllConditionsWithDetails = async () => {
    const conditions = await MedicalCondition.find();
    return conditions ? conditions.map(condition => sanitizeCondition(condition)) : [];

};

exports.updateMedicalCondition = async (conditionId, conditionDto) => {
    console.log('Updating condition:', conditionId);
    return await MedicalCondition.findByIdAndUpdate(conditionId, conditionDto, { new: true });
}

exports.findById = async (id) => {
    return await MedicalCondition.findById(id);

}
