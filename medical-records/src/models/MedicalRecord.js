const mongoose = require('mongoose');

const medicalRecordSchema = new mongoose.Schema({
  patientId: {
    type: String,
    required: true,
    index: true,
    ref: 'Patient' 
  },
  conditions: [{
    name: String,
    diagnosedDate: Date,
    status: String,
    notes: String,
    diagnosedBy: String
  }],
  allergies: [{
    allergen: String,
    severity: String,
    diagnosedDate: Date,
    notes: String
  }],
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