// Author: Hugo Barros

/**
 * This module defines the Mongoose schema and model for managing patient allergies in the medical records system.
 * It provides the data structure and validation rules for storing allergy information, including the allergen name,
 * severity level, and additional details. The schema ensures data consistency and proper formatting of allergy records.
 */

const mongoose = require('mongoose');

/**
 * Mongoose schema definition for Allergy
 * Defines the structure and validation rules for allergy documents
 * 
 * Methods available through the model:
 * - create(): Creates a new allergy record
 * - find(): Retrieves allergy records matching query criteria
 * - findOne(): Finds a single allergy record
 * - findById(): Retrieves an allergy by its ID
 * - updateOne(): Updates a single allergy record
 * - deleteOne(): Removes a single allergy record
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
 * Creates and exports the Allergy model
 * This model provides an interface for performing CRUD operations on allergy records
 * Inherits all Mongoose model methods for database interactions
 * @returns {Model} Mongoose model for Allergy documents
 */
module.exports = mongoose.model('Allergy', allergySchema);