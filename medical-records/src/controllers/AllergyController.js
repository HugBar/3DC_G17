const medicalRecordService = require('../services/AllergyService');
const AllergyDto = require('../dtos/AllergyDto');

exports.addAllergy = async (req, res) => {
    try {
        const { patientId } = req.params;
        const { allergen, severity, diagnosedDate, notes } = req.body;

        console.log('Adding allergy to patient', patientId);
        
        // Create DTO
        const allergyDto = new AllergyDto(allergen, severity, diagnosedDate, notes);
        
        // Add allergy using service
        const result = await medicalRecordService.addAllergy(patientId, allergyDto);
        
        res.status(201).json({
            message: 'Allergy added successfully',
            allergy: result
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