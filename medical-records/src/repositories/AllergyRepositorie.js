const MedicalRecord = require('../models/MedicalRecord');
const Allergy = require('../models/Allergy');

exports.findByPatientId = async (patientId) => {
    return await MedicalRecord.findOne({ patientId });
};

// Find allergy by allergen name
exports.findByAllergen = async (allergen) => {
    return await Allergy.findOne({ allergen });
};

// Add new allergy to patient's medical record
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

// Add allergy to allergy model
exports.addAllergyModel = async (allergy) => {
    return await Allergy.create(allergy);
};

// Get all allergies from allergy model
exports.getAllergies = async () => {
    return await Allergy.find();
};

exports.findByFilters = async (filters) => {
    const query = {};

    // Adiciona o filtro de allergen, se fornecido
    if (filters.allergen) {
        query.allergen = new RegExp(filters.allergen, 'i'); // Regex para busca parcial, ignorando maiúsculas/minúsculas
    }

    // Adiciona o filtro de severity, se fornecido
    if (filters.severity) {
        query.severity = filters.severity;
    }

    return Allergy.find(query);
}