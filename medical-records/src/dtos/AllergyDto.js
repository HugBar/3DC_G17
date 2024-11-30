class AllergyDto {
    constructor(allergen, severity, diagnosedDate, notes) {
        this.allergen = allergen;
        this.severity = severity;
        this.diagnosedDate = diagnosedDate;
        this.notes = notes;
    }
}

module.exports = AllergyDto;
