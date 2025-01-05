// Author: João Morais

/**
 * Data Transfer Object for searching medical records
 * Used to transfer search criteria between layers of the application
 * Allows filtering medical records by patient ID, condition name and allergy name
 */
class SearchMedicalRecordDto {
    /**
     * Creates a new SearchMedicalRecordDto
     * @param {string} patientId - The ID of the patient to search for
     * @param {string} conditionName - The name of the medical condition to filter by
     * @param {string} allergyName - The name of the allergy to filter by
     */
    constructor(patientId, conditionName, allergyName) {
        this.patientId = patientId;
        this.conditionName = conditionName;
        this.allergyName = allergyName;
    }

    /**
     * Validates the search criteria
     * Ensures required fields are present
     * @throws {Error} If patient ID is missing
     */
    validate() {
        if (!this.patientId) {
            throw new Error('Patient ID is required');
        }
    }
}

module.exports = SearchMedicalRecordDto; 