// Author: Matias Vitorino

/**
 * Data Transfer Object for Medical Condition search criteria
 * Used to transfer search parameters between layers of the application
 */
class SearchMedicalConditionDto {
    /**
     * Creates a new SearchMedicalConditionDto
     * @param {string} name - The name to search for
     * @param {string} severity - The severity level to filter by
     */
    constructor(name, severity) {
        this.name = name;
        this.severity = severity;
    }
}

module.exports = SearchMedicalConditionDto;