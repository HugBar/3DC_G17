const SpecializationService = require('../services/SpecializationService');
const SpecializationDto = require('../dtos/SpecializationDto');

exports.addSpecialization = async (req, res) => {
    try {
        const { name, description } = req.body;
        
        // Create DTO
        const specializationDto = new SpecializationDto(name, description);
        
        // Add specialization using service
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

exports.getAllSpecializations = async (req, res) => {
    try {
        const specializations = await SpecializationService.getAllSpecializations();
        res.status(200).json(specializations);
    } catch (error) {
        console.error('Error getting specializations:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};

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