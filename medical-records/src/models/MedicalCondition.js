// Author: Matias Vitorino

/**
 * This module defines the Mongoose schema and model for Medical Conditions.
 * It represents medical conditions in the system with fields for:
 * - name: The unique identifier name of the condition
 * - severity: The severity level (Low, Medium, High)
 * - description: Optional details about the condition
 * - createdDate: Timestamp of when the record was created
 */

const mongoose = require('mongoose');

/**
 * Mongoose schema definition for Medical Condition
 * Includes validation and default values for each field
 */
const medicalConditionSchema = new mongoose.Schema({
    name: {
        type: String,
        required: true,
        unique: true // Ensures each condition name is unique in the catalog
    },
    severity: {
        type: String,
        required: true,
        enum: ['Low', 'Medium', 'High'] // Restricts severity to these values
    },
    description: {
        type: String // Optional field for additional details
    },
    createdDate: {
        type: Date,
        default: Date.now // Automatically sets creation timestamp
    }
});

module.exports = mongoose.model('MedicalCondition', medicalConditionSchema);