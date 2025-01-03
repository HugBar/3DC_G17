// Author: Matias Vitorino

/**
 * This module provides API endpoints for managing medical conditions.
 * It handles the creation and search of medical conditions in the system.
 * Only administrators can add conditions, while doctors can search them.
 */

const MedicalConditionService = require('../services/MedicalConditionService');
const MedicalConditionDto = require('../dtos/MedicalConditionDto');
const SearchMedicalConditionDto = require('../dtos/SearchMedicalConditionDto');
const updateMedicalConditionDto = require('../dtos/UpdateMedicalConditionDto');
const UpdateMedicalConditionDto = require('../dtos/UpdateMedicalConditionDto');

/**
 * Adds a medical condition to a specific patient's record
 * @param {Object} req - Request object containing patientId and condition details
 * @param {Object} res - Response object
 * @returns {Object} JSON response with created condition or error message
 */
exports.addMedicalCondition = async (req, res) => {
    try {
        const { patientId } = req.params;
        const { name, severity, description } = req.body;

        console.log('Adding medical condition to patient', patientId);
        
        const medicalConditionDto = new MedicalConditionDto(name, severity, description);
        const result = await MedicalConditionService.addMedicalCondition(medicalConditionDto);
        
        res.status(201).json({
            message: 'Medical condition added successfully',
            medicalCondition: result
        });
    } catch (error) {
        console.error(error);
        if (error.message === 'Medical record not found') {
            res.status(404).json({ message: error.message });
        } else {
            res.status(500).json({ message: 'Internal server error' });
        }
    }
};

/**
 * Adds a new medical condition to the system catalog
 * Restricted to admin users only
 * @param {Object} req - Request object containing condition details
 * @param {Object} res - Response object
 * @returns {Object} JSON response with created condition or error message
 */
exports.addMedicalConditionModel = async (req, res) => {
    try {
        const { name, severity, description } = req.body;
        const medicalConditionDto = new MedicalConditionDto(name, severity, description);
        const result = await MedicalConditionService.addMedicalConditionModel(medicalConditionDto);
        
        res.status(201).json({
            message: 'Medical condition added successfully',
            medicalCondition: result
        });
    } catch (error) {
        console.error('Error adding medical condition:', error);
        if (error.message === 'Medical condition already exists') {
            res.status(409).json({ message: error.message });
        } else {
            res.status(500).json({ message: 'Internal server error' });
        }
    }
};

/**
 * Searches for medical conditions based on filters
 * Restricted to doctor users only
 * @param {Object} req - Request object containing search parameters
 * @param {Object} res - Response object
 * @returns {Object} JSON response with matching conditions or error message
 */
exports.searchMedicalConditions = async (req, res) => {
    try {
        const { name, severity } = req.query;
        const searchDto = new SearchMedicalConditionDto(name, severity);
        const conditions = await MedicalConditionService.searchMedicalConditions(searchDto);
        
        // Map response to include _id
        const conditionsWithIds = conditions.map(condition => ({
            _id: condition._id,
            name: condition.name,
            severity: condition.severity,
            description: condition.description
        }));

        res.status(200).json(conditionsWithIds);
    } catch (error) {
        console.error('Controller error:', error);
        res.status(500).json({ error: error.message });
    }
};


exports.getConditionDetails = async (req, res) => {
    try {
        const conditions = await MedicalConditionService.getAllConditions();
        res.status(200).json(conditions);
    } catch (error) {
        console.error('Error fetching conditions:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};

exports.updateMedicalCondition = async (req, res) => {
    try {
        const { id } = req.params;
        const { name, severity, description } = req.body;

        const conditionDto = new UpdateMedicalConditionDto(
            name || undefined,
            severity || undefined,
            description !== undefined ? description : undefined
        );

        const result = await MedicalConditionService.updateMedicalCondition(id, conditionDto);
        res.status(200).json({
            message: 'Medical condition updated successfully',
            medicalCondition: result
        });
    }
    catch (error) {
        console.error('Error updating condition:', error);
        if (error.message === 'Medical condition not found') {
            res.status(404).json({ message: error.message });
        } else if (error.message === 'Medical condition name already exists') {
            res.status(409).json({ message: error.message });
        } else {
            res.status(500).json({ message: 'Internal server error' });
        }
    }
};

exports.getMedicalCondition = async (req, res) => {
    try {
        const { id } = req.params;
        const condition = await MedicalConditionService.getMedicalCondition(id);
        if (!condition) {
            return res.status(404).json({ message: 'Medical condition not found' });
        }
        res.status(200).json(condition);
    } catch (error) {
        console.error('Error getting medical condition:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};