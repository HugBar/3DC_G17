// Author: João Morais

/**
 * This service module handles the business logic for medical specializations.
 * It provides methods for:
 * - Adding new specializations
 * - Retrieving all specializations
 * - Fetching specific specializations by ID
 * Acts as an intermediary between the controller and repository layers.
 */

const specializationRepository = require('../repositories/SpecializationRepository');
const SpecializationDto = require('../dtos/SpecializationDto');

/**
 * Service class that implements business logic for Specializations
 * Validates and processes data before database operations
 */
class SpecializationService {
    /**
     * Creates a new specialization after validating it doesn't already exist
     * @param {SpecializationDto} specializationDto - Data transfer object containing specialization details
     * @returns {Promise<Object>} The newly created specialization
     * @throws {Error} If specialization already exists or other errors occur
     */
    static async addSpecialization(specializationDto) {
        try {
            console.log("------------------------------------------")
            const existingSpecialization = await specializationRepository.findByName(specializationDto.name);
            if (existingSpecialization) {
                throw new Error('Specialization already exists');
            }

            return await specializationRepository.create({
                name: specializationDto.name,
                description: specializationDto.description
            });
        } catch (error) {
            throw error;
        }
    }

    /**
     * Retrieves all specializations from the database
     * @returns {Promise<Array>} Array of all specializations
     * @throws {Error} If database operation fails
     */
    static async getAllSpecializations() {
        try {
            return await specializationRepository.findAll();
        } catch (error) {
            throw error;
        }
    }

    /**
     * Retrieves a specific specialization by its ID
     * @param {string} id - The ID of the specialization to retrieve
     * @returns {Promise<Object>} The requested specialization
     * @throws {Error} If specialization not found or other errors occur
     */
    static async getSpecializationById(id) {
        try {
            const specialization = await specializationRepository.findById(id);
            if (!specialization) {
                throw new Error('Specialization not found');
            }
            return specialization;
        } catch (error) {
            throw error;
        }
    }

    static async searchSpecializations(SpecializationSearchDto){
        try{

            const specializations = await specializationRepository.searchSpecialization(SpecializationSearchDto);

            return specializations.map(specialization => new SpecializationDto(
                specialization.name,
                specialization.description
            ))
        } catch (error) {
            throw error;
        }    
    }

    /**
     * Deletes a specific specialization by its ID
     * @param {string} id - The ID of the specialization to delete
     * @returns {Promise<Object>} The deleted specialization
     * @throws {Error} If specialization not found or other errors occur
     */
    static async deleteSpecialization(id) {
        try {
            const specialization = await specializationRepository.findById(id);
            if (!specialization) {
                throw new Error('Specialization not found');
            }
            
            return await specializationRepository.delete(id);
        } catch (error) {
            throw error;
        }
    }

    /**
     * Updates a specific specialization by its ID
     * @param {string} id - The ID of the specialization to update
     * @param {SpecializationDto} specializationDto - The DTO containing updated specialization data
     * @returns {Promise<Object>} The updated specialization
     * @throws {Error} If specialization not found, name already exists, or other errors occur
     */
    static async updateSpecialization(id, specializationDto) {
        try {
            const existingSpecialization = await specializationRepository.findById(id);
            if (!existingSpecialization) {
                throw new Error('Specialization not found');
            }

            if (specializationDto.name !== existingSpecialization.name) {
                const duplicateSpecialization = await specializationRepository.findByName(specializationDto.name);
                if (duplicateSpecialization) {
                    throw new Error('Specialization with this name already exists');
                }
            }

            return await specializationRepository.update(id, {
                name: specializationDto.name,
                description: specializationDto.description,
                updatedAt: new Date()
            });
        } catch (error) {
            throw error;
        }
    }
}

module.exports = SpecializationService;