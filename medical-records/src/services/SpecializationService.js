// Author: João Morais

/**
 * This service module handles the business logic for medical specializations.
 * It provides methods for:
 * - Adding new specializations
 * - Retrieving all specializations
 * - Fetching specific specializations by ID
 * Acts as an intermediary between the controller and repository layers.
 */

const SpecializationRepository = require('../repositories/SpecializationRepository');
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
            const existingSpecialization = await SpecializationRepository.findByName(specializationDto.name);
            if (existingSpecialization) {
                throw new Error('Specialization already exists');
            }

            return await SpecializationRepository.create({
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
            return await SpecializationRepository.findAll();
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
            const specialization = await SpecializationRepository.findById(id);
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

            const specializations = await SpecializationRepository.searchSpecialization(SpecializationSearchDto);

            return specializations.map(specialization => new SpecializationDto(
                specialization.name,
                specialization.description
            ))
        } catch (error) {
            throw error;
        }    
    }
}

module.exports = SpecializationService;