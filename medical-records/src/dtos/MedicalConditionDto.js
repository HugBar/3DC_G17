// Author: Matias Vitorino

/**
 * Data Transfer Object for Medical Conditions
 * Used to transfer medical condition data between layers of the application
 */
class MedicalConditionDto {
    /**
     * Creates a new MedicalConditionDto
     * @param {string} name - The name of the medical condition
     * @param {string} severity - The severity level of the condition
     * @param {string} description - Detailed description of the condition
     */
    constructor(name, severity, description) {
        this.name = name;
        this.severity = severity;
        this.description = description;
    }
}

module.exports = MedicalConditionDto;