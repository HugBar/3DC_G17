class UpdateAllergyDto {
    constructor(allergen, severity, description, updateDate) {
        this.allergen = allergen;
        this.severity = severity;
        this.description = description;
        this.updateDate = updateDate;

    }
}

module.exports = UpdateAllergyDto;