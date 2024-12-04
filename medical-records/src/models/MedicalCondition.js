const mongoose = require('mongoose');

const medicalConditionSchema = new mongoose.Schema({
    name: {
        type: String,
        required: true,
        unique: true // Garantir que não haja condições médicas duplicadas no catálogo
    },
    severity: {
        type: String,
        required: true,
        enum: ['Low', 'Medium', 'High']
    },
    description: {
        type: String
    },
    createdDate: {
        type: Date,
        default: Date.now
    }
});

const MedicalCondition = mongoose.model('MedicalCondition', medicalConditionSchema);

module.exports = MedicalCondition;