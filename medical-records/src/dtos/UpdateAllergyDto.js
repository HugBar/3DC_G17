class UpdateAllergyDto {
    constructor(allergen, severity, description) {
        this.allergen = allergen;
        this.severity = severity;
        this.description = description;

    }
}

module.exports = UpdateAllergyDto;