// Author: João Morais

/**
 * This module provides methods for interacting with the Specialization model in the database.
 * It includes functionality for:
 * - Finding specializations by name
 * - Creating new specializations
 * - Retrieving all specializations
 * - Fetching specific specializations by ID
 */

const Specialization = require('../models/Specialization');

/**
 * Repository class that handles all database operations for Specializations
 * Provides an abstraction layer between the service and database
 */
class SpecializationRepository {
    constructor() {
        if (SpecializationRepository.instance) {
            return SpecializationRepository.instance;
        }
        SpecializationRepository.instance = this;
    }

    /**
     * Finds a specialization by its unique name
     * @param {string} name - The name of the specialization to find
     * @returns {Promise<Object>} The found specialization or null
     */
    async findByName(name) {
        return await Specialization.findOne({ name });
    }

    /**
     * Creates a new specialization in the database
     * @param {Object} specialization - The specialization data to create
     * @returns {Promise<Object>} The newly created specialization
     */
    async create(specialization) {
        const newSpecialization = new Specialization(specialization);
        return await newSpecialization.save();
    }

    /**
     * Retrieves all specializations from the database
     * @returns {Promise<Array>} Array of all specializations
     */
    async findAll() {
        return await Specialization.find();
    }

    /**
     * Finds a specialization by its unique identifier
     * @param {string} id - The ID of the specialization to find
     * @returns {Promise<Object>} The found specialization or null
     */
    async findById(id) {
        return await Specialization.findById(id);
    }

    async searchSpecialization (filters){
        const query = {};

        if(filters.name){
            query.name = new RegExp(filters.name, 'i');
        } if(filters.description){
            query.description = new RegExp(filters.description, 'i');
        }

        return Specialization.find(query);
    }

    /**
     * Deletes a specific specialization by its ID
     * @param {string} id - The ID of the specialization to delete
     * @returns {Promise<Object>} The deleted specialization
     */
    async delete(id) {
        return await Specialization.findByIdAndDelete(id);
    }

    /**
     * Updates a specialization by its ID
     * @param {string} id - The ID of the specialization to update
     * @param {Object} updateData - The data to update
     * @returns {Promise<Object>} The updated specialization
     */
    async update(id, updateData) {
        return await Specialization.findByIdAndUpdate(
            id,
            updateData,
            { new: true } // Returns the updated document
        );
    }
}

// Create and export a singleton instance
const specializationRepository = new SpecializationRepository();
module.exports = specializationRepository;