// Author: Hugo Barros

/**
 * Data Transfer Object for creating new allergies
 * Used to transfer allergy creation data between layers of the application
 * Contains the essential information needed to create a new allergy record
 */
class CreatAllergyDto {
    /**
     * Creates a new CreatAllergyDto
     * @param {string} allergen - The name/type of the allergen
     * @param {string} severity - The severity level of the allergy
     * @param {string} description - Detailed description of the allergy
     */
    constructor(allergen, severity, description) {
        this.allergen = allergen;
        this.severity = severity; 
        this.description = description;
    }
}

module.exports = CreatAllergyDto;