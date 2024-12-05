// Author: João Morais

// This Data Transfer Object (DTO) is used to transfer specialization data between layers
// of the application. It encapsulates the name and description of a medical specialization.

class SpecializationDto {
    /**
     * Creates a new SpecializationDto instance
     * @param {string} name - The name of the specialization
     * @param {string} description - A description of what the specialization entails
     */
    constructor(name, description) {
        this.name = name;
        this.description = description;
    }
}

module.exports = SpecializationDto; 