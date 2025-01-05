// Author: Hugo Barros
/**
 * This module provides API endpoints for managing allergies in the medical records system.
 * It handles the creation, search, update and deletion of allergies.
 * Only administrators can add/update/delete allergies, while doctors can search them.
 */

const AllergyService = require('../services/AllergyService');
const AllergyDto = require('../dtos/AllergyDto');
const AllergySearchDto = require('../dtos/AllergySearchDto');
const UpdateAllergyDto = require('../dtos/UpdateAllergyDto');
const CreatAllergyDto = require('../dtos/CreatAllergyDto');

/**
 * Adds a new allergy to the system catalog
 * Restricted to admin users only
 * @param {Object} req - Request object containing allergy details
 * @param {Object} res - Response object
 * @returns {Object} JSON response with created allergy or error message
 */
exports.addAllergyModel = async (req, res) => {
    try {
        const { allergen, severity, description } = req.body;

        // Create DTO
        const allergyDto = new CreatAllergyDto(allergen, severity, description);
        
        // Add allergy using service
        const result = await AllergyService.addAllergyModel(allergyDto);
        
        res.status(201).json({
            message: 'Allergy added successfully',
            allergy: result
        });
    } catch (error) {
        console.error('Error adding allergy:', error);
        if (error.message === 'Allergy already exists') {
            res.status(409).json({ message: error.message });
        } else {
            res.status(500).json({ message: 'Internal server error' });
        }
    }
};

/**
 * Searches for allergies based on provided filters
 * Accessible by doctors and admins
 * @param {Object} req - Request object containing search parameters
 * @param {Object} res - Response object
 * @returns {Object} JSON response with matching allergies or error message
 */
exports.searchAllergies = async (req, res) => {
    try {
        const { allergen, severity } = req.query;

        console.log("----------------------------------")

        // Create filters object with only provided parameters
        const filters = {};
        if (allergen) {
            filters.allergen = allergen;
        }
        if (severity) {
            filters.severity = severity;
        }

        // Create search DTO with filters
        const allergySearchDto = new AllergySearchDto(allergen, severity);

        const allergies = await AllergyService.searchAllergies(allergySearchDto);
        res.status(200).json(allergies);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
};

/**
 * Retrieves details of all allergies in the system
 * @param {Object} req - Request object
 * @param {Object} res - Response object
 * @returns {Object} JSON response with all allergies or error message
 */
exports.getAllergyDetails = async (req, res) => {
    try {
        const allergies = await AllergyService.getAllAllergies();
        res.status(200).json(allergies);
    } catch (error) {
        console.error('Error fetching allergies:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};

/**
 * Updates an existing allergy in the system
 * Restricted to admin users only
 * @param {Object} req - Request object containing updated allergy details
 * @param {Object} res - Response object
 * @returns {Object} JSON response with success message or error
 */
exports.updateAllergy = async (req, res) => {
    try {
        const { id } = req.params;
        const { allergen, severity, description } = req.body;

        const updateDate = new Date();

        const allergyDto = new UpdateAllergyDto(allergen, severity, description, updateDate);
        console.log(allergyDto);

        const result = await AllergyService.updateAllergy(id, allergyDto);
        res.status(200).json({ message: result });
    } catch (error) {
        console.error('Error updating allergy:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
}

/**
 * Deletes an allergy from the system
 * Restricted to admin users only
 * @param {Object} req - Request object containing allergy ID
 * @param {Object} res - Response object
 * @returns {Object} JSON response with success message or error
 */
exports.deleteAllergy = async (req, res) => {
    try {
        const { id } = req.params;

        console.log("----------------------------------");
        console.log("allergen: ", id);
        const result = await AllergyService.deleteAllergy(id);
        res.status(200).json({ message: result });
    } catch (error) {
        console.error('Error deleting allergy:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};
