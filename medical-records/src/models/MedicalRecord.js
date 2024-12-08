const mongoose = require('mongoose');

const medicalRecordSchema = new mongoose.Schema({
    patientId: {
        type: String,
        required: true,
        unique: true,
        index: true,
        ref: 'Patient'
    },
    conditions: [{
        name: {
            type: String,
            required: true
        },
        severity: {
            type: String,
            required: true,
            enum: ['Low', 'Medium', 'High']
        }
    }],
    allergies: [{
        name: {
            type: String,
            required: true
        },
        severity: {
            type: String,
            required: true,
            enum: ['Low', 'Medium', 'High']
        }
    }],
    lastUpdated: {
        type: Date,
        default: Date.now
    }
}, { timestamps: true });

module.exports = mongoose.model('MedicalRecord', medicalRecordSchema);