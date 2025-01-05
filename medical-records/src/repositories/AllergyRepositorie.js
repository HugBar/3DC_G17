// Author: Hugo Barros

/**
 * Repository module for managing allergy-related database operations
 * Handles CRUD operations for allergies in both the Allergy model and MedicalRecord model
 * Provides methods for searching, creating, updating and deleting allergy records
 */

const MedicalRecord = require('../models/MedicalRecord');
const Allergy = require('../models/Allergy');

/**
 * Sanitizes allergy data for response
 * @param {Object} allergy - The allergy document to sanitize
 * @returns {Object} Sanitized allergy object with standardized field names
 */
const sanitizeAllergy = (allergy) => ({
    _id: allergy._id,
    name: allergy.allergen,
    severity: allergy.severity
});

/**
 * Finds a medical record by patient ID
 * @param {string} patientId - The ID of the patient
 * @returns {Promise<Object>} The medical record document
 */
exports.findByPatientId = async (patientId) => {
    return await MedicalRecord.findOne({ patientId });
};

/**
 * Finds an allergy by allergen name
 * @param {string} allergen - The name of the allergen
 * @returns {Promise<Object>} The allergy document
 */
exports.findByAllergen = async (allergen) => {
    return await Allergy.findOne({ allergen });
};

/**
 * Adds a new allergy to a patient's medical record
 * @param {Object} allergyDto - The allergy data to add
 * @returns {Promise<Object>} The added allergy data
 */
exports.addAllergy = async (allergyDto) => {
    try {
        const medicalRecord = await Allergy.findOne({ patientId });
        
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

/**
 * Creates a new allergy in the Allergy model
 * @param {Object} allergy - The allergy data to create
 * @returns {Promise<Object>} The created allergy document
 */
exports.addAllergyModel = async (allergy) => {
    return await Allergy.create(allergy);
};

/**
 * Retrieves all allergies from the Allergy model
 * @returns {Promise<Array>} Array of all allergy documents
 */
exports.getAllergies = async () => {
    return await Allergy.find();
};

/**
 * Searches allergies using provided filters
 * @param {Object} filters - Search criteria including allergen and severity
 * @returns {Promise<Array>} Array of matching allergy documents
 */
exports.findByFilters = async (filters) => {
    const query = {};

    if (filters.allergen) {
        query.allergen = new RegExp(filters.allergen, 'i');
    }

    if (filters.severity) {
        query.severity = filters.severity;
    }

    return Allergy.find(query);
}

/**
 * Gets all allergies with sanitized details
 * @returns {Promise<Array>} Array of sanitized allergy objects
 */
exports.getAllAllergiesWithDetails = async () => {
    const allergies = await Allergy.find();
    return allergies ? allergies.map(allergy => sanitizeAllergy(allergy)) : [];
};

/**
 * Deletes an allergy by allergen name
 * @param {string} allergen - The name of the allergen to delete
 * @returns {Promise<string>} Success or failure message
 */
exports.deleteAllergy = async (allergen) => {
    console.log(allergen);
    const result = await Allergy.deleteOne({ allergen });
    return result.deletedCount ? 'Allergy deleted successfully' : 'Allergy not found';
};

/**
 * Updates an existing allergy
 * @param {string} id - The ID of the allergy to update
 * @param {Object} allergyDto - The updated allergy data
 * @returns {Promise<Object>} The updated allergy document
 */
exports.updateAllergy = async (id, allergyDto) => {
    console.log(allergyDto);

    return await Allergy.findByIdAndUpdate(id, allergyDto, { new: true });
}

/**
 * Finds an allergy by its ID
 * @param {string} id - The ID of the allergy
 * @returns {Promise<Object>} The allergy document
 */
exports.findById = async (id) => {
    return await Allergy.findById(id);
}