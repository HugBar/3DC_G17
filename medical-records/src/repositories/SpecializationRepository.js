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
        }

        return Specialization.find(query);
    }

}



module.exports = new SpecializationRepository();