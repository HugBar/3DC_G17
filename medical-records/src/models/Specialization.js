// Author: João Morais

/**
 * This module defines the Mongoose schema and model for the Specialization entity.
 * It represents medical specializations in the system with fields for:
 * - name: The unique identifier name of the specialization
 * - description: Optional details about the specialization
 * - createdAt: Timestamp of when the record was created
 * - updatedAt: Timestamp of the last update
 */

const mongoose = require('mongoose');

/**
 * Mongoose schema definition for Specialization
 * Includes validation and default values for each field
 */
const specializationSchema = new mongoose.Schema({
    name: {
        type: String,
        required: true,
        unique: true // Ensures that each specialization name is unique
    },
    description: {
        type: String,
        required: false // Description is optional
    },
    createdAt: {
        type: Date,
        default: Date.now // Automatically sets the creation date
    },
    updatedAt: {
        type: Date,
        default: Date.now // Automatically sets the update date
    }
});

/**
 * Export the Specialization model based on the schema
 * This model provides an interface for database operations
 */
module.exports = mongoose.model('Specialization', specializationSchema);