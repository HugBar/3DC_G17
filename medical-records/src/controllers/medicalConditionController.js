const MedicalConditionService = require('../services/MedicalConditionService');
const MedicalConditionDto = require('../dtos/MedicalConditionDto');
const SearchMedicalConditionDto = require('../dtos/SearchMedicalConditionDto');

exports.addMedicalCondition = async (req, res) => {
    try {
        const { patientId } = req.params;
        const { name, severity, description } = req.body;

        console.log('Adding medical condition to patient', patientId);
        
        // Create DTO
        const medicalConditionDto = new MedicalConditionDto(name, severity, description);
        
        // Add medical condition using service
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

exports.addMedicalConditionModel = async (req, res) => {
    try {
        const { name, severity, description } = req.body;

        // Create DTO
        const medicalConditionDto = new MedicalConditionDto(name, severity, description);
        
        // Add medical condition using service
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

exports.searchMedicalConditions = async (req, res) => {
    try {
        const { name, severity } = req.query;

        // Criar um DTO de busca com os filtros
        const searchDto = new SearchMedicalConditionDto(name, severity);

        const conditions = await MedicalConditionService.searchMedicalConditions(searchDto);
        res.status(200).json(conditions);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
};