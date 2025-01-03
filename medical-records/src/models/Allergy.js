// Author: [Your Name]

/**
 * This module defines the Mongoose schema and model for the Allergy entity.
 * It represents allergies in the system with fields for:
 * - allergen: The unique identifier name of the allergy
 * - severity: The severity level of the allergy (Low, Medium, High)
 * - description: Optional details about the allergy
 * - createdDate: Timestamp of when the record was created
 * - updatedDate: Timestamp of the last update
 */

const mongoose = require('mongoose');

/**
 * Mongoose schema definition for Allergy
 * Includes validation and default values for each field
 */
const allergySchema = new mongoose.Schema({
    allergen: {
        type: String,
        required: true,
        unique: true // Ensures that each allergy name is unique in the catalog
    },
    severity: {
        type: String,
        required: true,
        enum: ['Low', 'Medium', 'High'] // Restricts severity to these three values
    },
    description: {
        type: String,
        required: false // Description is optional
    },
    createdDate: {
        type: Date,
        default: Date.now // Automatically sets the creation date
    },
    updatedDate: {
        type: Date,
        default: Date.now // Automatically sets the update date
    }
});

/**
 * Export the Allergy model based on the schema
 * This model provides an interface for database operations
 */
module.exports = mongoose.model('Allergy', allergySchema);