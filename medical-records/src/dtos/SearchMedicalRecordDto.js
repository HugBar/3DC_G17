class SearchMedicalRecordDto {
    constructor(patientId, conditionName, allergyName) {
        this.patientId = patientId;
        this.conditionName = conditionName;
        this.allergyName = allergyName;
    }

    validate() {
        if (!this.patientId) {
            throw new Error('Patient ID is required');
        }
    }
}

module.exports = SearchMedicalRecordDto; 