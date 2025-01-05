// Author: Hugo Barros

/**
 * Data Transfer Object for searching medical specializations
 * Used to transfer search criteria between layers of the application
 * Allows filtering specializations by name and description
 */
class SearchSpecializationDto {
    /**
     * Creates a new SearchSpecializationDto
     * @param {string} name - The name of the specialization to search for
     * @param {string} description - The description text to filter by
     */
    constructor(name, description) {
        this.name = name;
        this.description = description;
    }
}

module.exports = SearchSpecializationDto;