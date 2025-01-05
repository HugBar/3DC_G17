// Author: João Morais

/**
 * This module defines the Mongoose schema and model for Medical Records.
 * It represents a patient's complete medical record including their conditions
 * and allergies. The schema provides structure for storing and managing patient
 * medical history with proper validation and relationships.
 */

const mongoose = require('mongoose');

/**
 * Mongoose schema definition for Medical Record
 * Includes validation rules and relationships for:
 * - patientId: References the Patient model and must be unique
 * - conditions: Array of medical conditions with name and severity
 * - allergies: Array of allergies with name and severity
 * - lastUpdated: Timestamp of last modification
 * 
 * Methods available through the model:
 * - create(): Creates a new medical record
 * - find(): Retrieves medical records matching criteria
 * - findOne(): Finds a single medical record
 * - findById(): Retrieves a medical record by ID
 * - updateOne(): Updates a single medical record
 * - deleteOne(): Removes a single medical record
 */
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

/**
 * Creates and exports the MedicalRecord model
 * This model provides an interface for performing CRUD operations on medical records
 * Inherits all Mongoose model methods for database interactions
 * @returns {Model} Mongoose model for MedicalRecord documents
 */
module.exports = mongoose.model('MedicalRecord', medicalRecordSchema);