const MedicalRecord = require('../models/MedicalRecord');
const MedicalCondition = require('../models/MedicalCondition');

// Find medical record by patient ID
exports.findByPatientId = async (patientId) => {
    return await MedicalRecord.findOne({ patientId });
};

// Find medical condition by name
exports.findByName = async (name) => {
    return await MedicalCondition.findOne({ name });
};

// Add new medical condition to patient's medical record
exports.addMedicalCondition = async (medicalConditionDto) => {
    try {
        const medicalRecord = await MedicalRecord.findOne({ patientId });
        
        if (!medicalRecord) {
            throw new Error('Medical record not found');
        }

        medicalRecord.medicalConditions.push(medicalConditionDto);
        await medicalRecord.save();

        return medicalConditionDto;
    } catch (error) {
        throw error;
    }
};

// Add medical condition to medical condition model
exports.addMedicalConditionModel = async (medicalCondition) => {
    return await MedicalCondition.create(medicalCondition);
};

// Get all medical conditions from medical condition model
exports.getAllMedicalConditions = async () => {
    return await MedicalCondition.find();
};

// Find medical conditions by filters
exports.findByFilters = async (filters) => {
    const query = {};

    // Adiciona o filtro de name, se fornecido
    if (filters.name) {
        query.name = new RegExp(filters.name, 'i'); // Regex para busca parcial, ignorando maiúsculas/minúsculas
    }

    // Adiciona o filtro de severity, se fornecido
    if (filters.severity) {
        query.severity = filters.severity;
    }

    return MedicalCondition.find(query);
};