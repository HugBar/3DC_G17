// Author: Hugo Barros

/**
 * Service layer for managing allergy-related business logic.
 * Handles operations for allergies including:
 * - Adding allergies to patient medical records
 * - Managing the allergy catalog/model
 * - Searching and retrieving allergy records
 * - Updating and deleting allergy information
 */

const MedicalRecord = require('../models/MedicalRecord');
const Allergy = require('../models/Allergy');
const AllergyDto = require('../dtos/AllergyDto');
const AlergyRepository = require('../repositories/AllergyRepositorie');

class AllergyService {
    /**
     * Adds a new allergy to a patient's medical record
     * @param {Object} allergyDto - Data transfer object containing allergy details
     * @returns {Promise<Object>} The newly added allergy
     */
    async addAllergy(allergyDto) {
        try {
            const medicalRecord = await MedicalRecord.findOne({ patientId });
            console.log(medicalRecord);
            
            if (!medicalRecord) {
                throw new Error('Medical record not found');
            }

            const newAllergy = {
                allergen: allergyDto.allergen,
                severity: allergyDto.severity,
                description: allergyDto.desription
            };

            const allergy = await AlergyRepository.addAllergy(newAllergy);        
            
            return allergy;
        } catch (error) {
            throw error;
        }
    }

    /**
     * Creates a new allergy in the allergy catalog/model
     * @param {Object} allergyDto - Data transfer object containing allergy details
     * @returns {Promise<Object>} The newly created allergy
     */
    async addAllergyModel(allergyDto) {
        try {
            const allergy = await Allergy.findOne({ allergen: allergyDto.allergen });
            if (allergy) {
                throw new Error('Allergy already exists');
            }

            const newAllergy = new Allergy({
                allergen: allergyDto.allergen,
                severity: allergyDto.severity,
                description: allergyDto.description
            });

            console.log(newAllergy);

            const addedAllergy = await AlergyRepository.addAllergyModel(newAllergy);

            return addedAllergy;

        } catch (error) {
            throw error;
        }
    }

    /**
     * Searches for allergies based on provided filters
     * @param {Object} allergySearchDto - Search criteria for filtering allergies
     * @returns {Promise<Array>} Array of matching allergies converted to DTOs
     */
    async searchAllergies(allergySearchDto) {
        try {
            const allergies = await AlergyRepository.findByFilters(allergySearchDto);
            return allergies.map(allergy => new AllergyDto(
                allergy.id,
                allergy.allergen,
                allergy.severity,
                allergy.description
            ));
        } catch (error) {
            throw error;
        }
    }

    /**
     * Retrieves all allergies from the system
     * @returns {Promise<Array>} Array of all allergies with sanitized details
     */
    async getAllAllergies() {
        try {
            const allergies = await AlergyRepository.getAllAllergiesWithDetails();
            if (!allergies || allergies.length === 0) {
                return [];
            }
            return allergies;
        } catch (error) {
            throw error;
        }
    }

    /**
     * Deletes an allergy from the system by ID
     * @param {string} allergyId - The ID of the allergy to delete
     * @returns {Promise<Object>} Result of the deletion operation
     */
    async deleteAllergy(allergyId) {
        try {
            const allergy = await AlergyRepository.deleteAllergy(allergyId);
            return allergy;
        } catch (error) {
            throw error;
        }
    }

    /**
     * Updates an existing allergy's information
     * @param {string} id - The ID of the allergy to update
     * @param {Object} allergyDto - Updated allergy information
     * @returns {Promise<Object>} The updated allergy as a DTO
     */
    async updateAllergy(id, allergyDto) {
        try {
            const allergyIdExists = await AlergyRepository.findById(id);
            if (!allergyIdExists) {
                throw new Error('Allergy not found');
            }
        
            const allergy = await AlergyRepository.updateAllergy(id, allergyDto);

            const updatedAllergy = new AllergyDto(
                allergy.id,
                allergy.allergen,
                allergy.severity,
                allergy.description
            );

            return updatedAllergy;
        } catch (error) {
            throw error;
        }
    }
}

module.exports = new AllergyService();
