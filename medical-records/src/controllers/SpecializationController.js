// Author: João Morais

/**
 * This module provides API endpoints for managing medical specializations.
 * It includes functionality for adding new specializations, retrieving all specializations,
 * and fetching specific specializations by their ID.
 */

const SpecializationService = require('../services/SpecializationService');
const SpecializationDto = require('../dtos/SpecializationDto');
const SpecializationSearchDto = require('../dtos/SearchSpecializationDto');

/**
 * Adds a new medical specialization
 * @param {Object} req - Express request object containing specialization details in body
 * @param {Object} res - Express response object
 * @returns {Object} JSON response with created specialization or error message
 */
exports.addSpecialization = async (req, res) => {
    try {
        const { name, description } = req.body;
        
        // Create DTO to transfer data
        const specializationDto = new SpecializationDto(name, description);
        
        // Add specialization using service layer
        const result = await SpecializationService.addSpecialization(specializationDto);
        
        res.status(201).json({
            message: 'Specialization added successfully',
            specialization: result
        });
    } catch (error) {
        console.error('Error adding specialization:', error);
        if (error.message === 'Specialization already exists') {
            res.status(409).json({ message: error.message });
        } else {
            res.status(500).json({ message: 'Internal server error' });
        }
    }
};

/**
 * Retrieves all medical specializations
 * @param {Object} req - Express request object
 * @param {Object} res - Express response object
 * @returns {Object} JSON response with array of specializations or error message
 */
exports.getAllSpecializations = async (req, res) => {
    try {
        const specializations = await SpecializationService.getAllSpecializations();
        res.status(200).json(specializations);
    } catch (error) {
        console.error('Error getting specializations:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};

/**
 * Retrieves a specific medical specialization by its ID
 * @param {Object} req - Express request object containing specialization ID in params
 * @param {Object} res - Express response object
 * @returns {Object} JSON response with requested specialization or error message
 */
exports.getSpecializationById = async (req, res) => {
    try {
        const { id } = req.params;
        const specialization = await SpecializationService.getSpecializationById(id);
        res.status(200).json(specialization);
    } catch (error) {
        console.error('Error getting specialization:', error);
        if (error.message === 'Specialization not found') {
            res.status(404).json({ message: error.message });
        } else {
            res.status(500).json({ message: 'Internal server error' });
        }
    }
}; 

exports.searchSpecialization = async (req, res) => {
    try {
        const { name} = req.query;

        // Create filters object
        const filters = {};
        if (name) filters.name = name;

        // Create search DTO
        const specializationSearchDto = new SpecializationSearchDto(name);

        const specializations = await SpecializationService.searchSpecializations(specializationSearchDto);
        res.status(200).json(specializations);
    } catch (error) {
        console.error('Error searching specializations:', error);
        res.status(500).json({ error: error.message });
    }
};
