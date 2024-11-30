const mongoose = require('mongoose');

const allergySchema = new mongoose.Schema({
    allergen: {
        type: String,
        required: true
    },
    severity: {
        type: String,
        required: true,
        enum: ['Low', 'Medium', 'High']
    },
    diagnosedDate: {
        type: Date,
        required: true
    },
    notes: {
        type: String
    }
});

const medicalRecordSchema = new mongoose.Schema({
    patientId: {
        type: String,
        required: true,
        unique: true,
        index: true,
        ref: 'Patient' // References patient ID from .NET backend
    },
    conditions: [{
        name: String,
        diagnosedDate: Date,
        status: String,
        notes: String,
        diagnosedBy: String
    }],
    allergies: [allergySchema],
    medications: [{
        name: String,
        dosage: String,
        frequency: String,
        startDate: Date,
        endDate: Date,
        prescribedBy: String
    }],
    lastUpdated: {
        type: Date,
        default: Date.now
    }
}, { timestamps: true });

module.exports = mongoose.model('MedicalRecord', medicalRecordSchema);