// Author: Joao Morais

/**
 * This module provides API endpoints for managing medical records.
 * It handles the creation, retrieval and updating of patient medical records.
 * Only authorized healthcare providers can access and modify these records.
 */

const MedicalRecordRepository = require('../repositories/MedicalRecordRepository');
const UpdateMedicalRecordDto = require('../dtos/UpdateMedicalRecordDto');
const MedicalRecordService = require('../services/MedicalRecordService');
const SearchMedicalRecordDto = require('../dtos/SearchMedicalRecordDto');

/**
 * Creates a new medical record for a patient
 * @param {Object} req - Request object containing the medical record data
 * @param {Object} res - Response object
 * @returns {Object} JSON response with created record or error message
 */
exports.createMedicalRecord = async (req, res) => {
    try {
        const medicalRecord = await MedicalRecordRepository.create(req.body);
        res.status(201).json(medicalRecord);
    } catch (error) {
        res.status(400).json({ error: error.message });
    }
};

/**
 * Retrieves all medical records from the system
 * @param {Object} req - Request object
 * @param {Object} res - Response object
 * @returns {Object} JSON response with array of medical records or error message
 */
exports.getAllMedicalRecords = async (req, res) => {
    try {
        const records = await MedicalRecordRepository.findAll();
        res.json(records);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
};

/**
 * Retrieves a specific medical record by patient ID
 * @param {Object} req - Request object containing patientId parameter
 * @param {Object} res - Response object
 * @returns {Object} JSON response with medical record or error message
 */
exports.getMedicalRecordByPatientId = async (req, res) => {
    try {
        const record = await MedicalRecordRepository.findByPatientId(req.params.patientId);
        if (!record) {
            return res.status(404).json({ message: 'Medical record not found' });
        }
        res.json(record);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
};

/**
 * Updates or creates a patient's medical record with new conditions and allergies
 * @param {Object} req - Request object containing patientId and update data
 * @param {Object} res - Response object
 * @returns {Object} JSON response with updated record or error message
 */
exports.updatePatientConditionsAndAllergies = async (req, res) => {
    try {
        const { patientId } = req.params;
        const { conditions, allergies } = req.body;
        
        const updateDto = new UpdateMedicalRecordDto(conditions, allergies);
        
        const existingRecord = await MedicalRecordRepository.findByPatientId(patientId);
        const isNewRecord = !existingRecord;
        
        const updatedRecord = await MedicalRecordService.updatePatientConditionsAndAllergies(
            patientId, 
            updateDto
        );

        res.status(isNewRecord ? 201 : 200).json({
            message: isNewRecord ? 
                'Medical record created successfully' : 
                'Medical record updated successfully',
            record: updatedRecord
        });
    } catch (error) {
        console.error('Error updating medical record:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};

/**
 * Searches for a medical record with the specified filters
 * @param {Object} req - Request object containing query parameters
 * @param {Object} res - Response object
 * @returns {Object} JSON response with the searched medical record or error message
 */
exports.searchMedicalRecord = async (req, res) => {
    try {
        const { patientId, conditionName, allergyName } = req.query;
        const searchDto = new SearchMedicalRecordDto(patientId, conditionName, allergyName);
        
        const result = await MedicalRecordService.searchMedicalRecord(searchDto);
        
        if (!result) {
            return res.status(404).json({ message: 'Medical record not found' });
        }

        res.status(200).json(result);
    } catch (error) {
        console.error('Error searching medical record:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};