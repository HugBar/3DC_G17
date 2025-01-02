class AllergyDto {
    constructor(id, allergen, severity, description) {
        this.id = id;
        this.allergen = allergen;
        this.severity = severity;
        this.description = description;
    }

}

module.exports = AllergyDto;
