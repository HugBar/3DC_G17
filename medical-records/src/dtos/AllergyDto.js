// Author: Hugo Barros

/**
 * Data Transfer Object for Allergies
 * Used to transfer allergy data between layers of the application
 * Contains core allergy information like allergen, severity and description
 */
class AllergyDto {
    /**
     * Creates a new AllergyDto
     * @param {string} id - The unique identifier of the allergy
     * @param {string} allergen - The name/type of the allergen
     * @param {string} severity - The severity level of the allergy
     * @param {string} description - Detailed description of the allergy
     */
    constructor(id, allergen, severity, description) {
        this.id = id;
        this.allergen = allergen;
        this.severity = severity;
        this.description = description;
    }
}

module.exports = AllergyDto;
