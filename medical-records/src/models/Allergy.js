const mongoose = require('mongoose');

const allergySchema = new mongoose.Schema({
    allergen: {
        type: String,
        required: true,
        unique: true // Garantir que não haja alergias duplicadas no catálogo
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

const Allergy = mongoose.model('Allergy', allergySchema);

module.exports = Allergy;
