const AllergyService = require('../services/AllergyService');
const AllergyDto = require('../dtos/AllergyDto');
const AllergySearchDto = require('../dtos/AllergySearchDto');
const UpdateAllergyDto = require('../dtos/UpdateAllergyDto');

// Add allergy to allergy model

exports.addAllergyModel = async (req, res) => {
    try {
        const { allergen, severity, description } = req.body;

        // Create DTO
        const allergyDto = new AllergyDto(allergen, severity, description);
        
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

// Search for allergies in the allergy model

exports.searchAllergies = async (req, res) => {
    try {
        const { allergen, severity } = req.query;

        console.log("----------------------------------")

        // Cria um objeto de filtros apenas com os parâmetros fornecidos
        const filters = {};
        if (allergen) {
            filters.allergen = allergen;
        }
        if (severity) {
            filters.severity = severity;
        }

        // Cria um DTO de busca com os filtros
        const allergySearchDto = new AllergySearchDto(allergen, severity);

        const allergies = await AllergyService.searchAllergies(allergySearchDto);
        res.status(200).json(allergies);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
};

exports.getAllergyDetails = async (req, res) => {
    try {
        const allergies = await AllergyService.getAllAllergies();
        res.status(200).json(allergies);
    } catch (error) {
        console.error('Error fetching allergies:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
};

exports.updateAllergy = async (req, res) => {
    try {
        const { id } = req.params;
        const { allergen, severity, description } = req.body;


        const allergyDto = new UpdateAllergyDto(allergen, severity, description);
        console.log(allergyDto);

        const result = await AllergyService.updateAllergy(id, allergyDto);
        res.status(200).json({ message: result });
    } catch (error) {
        console.error('Error updating allergy:', error);
        res.status(500).json({ message: 'Internal server error' });
    }
}

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
