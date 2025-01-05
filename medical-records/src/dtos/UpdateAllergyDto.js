// Author: Hugo Barros

/**
 * Data Transfer Object for updating existing allergies
 * Used to transfer allergy update data between layers of the application
 * Contains the information needed to modify an existing allergy record
 */
class UpdateAllergyDto {
    /**
     * Creates a new UpdateAllergyDto
     * @param {string} allergen - The name/type of the allergen to update
     * @param {string} severity - The new severity level of the allergy
     * @param {string} description - Updated description of the allergy
     * @param {Date} updateDate - The date when the update was made
     */
    constructor(allergen, severity, description, updateDate) {
        this.allergen = allergen;
        this.severity = severity;
        this.description = description;
        this.updateDate = updateDate;
    }
}

module.exports = UpdateAllergyDto;