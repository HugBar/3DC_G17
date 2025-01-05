// Author: Matias Vitorino

/**
 * Data Transfer Object for updating existing medical conditions
 * Used to transfer medical condition update data between layers of the application
 * Contains the information needed to modify an existing medical condition record
 */
class UpdateMedicalConditionDto {
  /**
   * Creates a new UpdateMedicalConditionDto
   * @param {string} name - The name of the medical condition to update
   * @param {string} severity - The new severity level of the condition
   * @param {string} description - Updated description of the medical condition
   */
  constructor(name, severity, description) {
    this.name = name;
    this.severity = severity;
    this.description = description;
  }
}

module.exports = UpdateMedicalConditionDto;