class UpdateMedicalRecordDto {
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