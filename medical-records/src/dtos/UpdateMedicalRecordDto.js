// Author: João Morais

// This Data Transfer Object (DTO) is used to transfer medical record update data between layers
// of the application. It encapsulates arrays of medical conditions and allergies with their
// respective names and severity levels.

class UpdateMedicalRecordDto {
    /**
     * Creates a new UpdateMedicalRecordDto instance
     * @param {Array} conditions - Array of medical conditions with name and severity
     * @param {Array} allergies - Array of allergies with name and severity
     */
    constructor(conditions, allergies) {
        this.conditions = conditions.map(condition => ({
            name: condition.name,
            severity: condition.severity
        }));
        
        this.allergies = allergies.map(allergy => ({
            name: allergy.name,
            severity: allergy.severity
        }));
    }
}

module.exports = UpdateMedicalRecordDto;