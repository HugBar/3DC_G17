// Author: Hugo Barros

/**
 * Data Transfer Object for searching allergies
 * Used to transfer search criteria between layers of the application
 * Allows filtering allergies by allergen type and severity level
 */
class AllergySearchDto {
    /**
     * Creates a new AllergySearchDto
     * @param {string} allergen - The allergen name/type to search for
     * @param {string} severity - The severity level to filter by
     * @param {string} description - The description to search for
     */
    constructor(allergen, severity, description) {
        this.allergen = allergen;
        this.severity = severity;
    }
}

module.exports = AllergySearchDto;
