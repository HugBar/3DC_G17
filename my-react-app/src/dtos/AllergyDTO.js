export class AllergyDTO {
    constructor(allergen, severity, description) {
        this.allergen = allergen;
        this.severity = severity;
        this.description = description;
    }

    static fromResponse(data) {
        return new AllergyDTO(
            data.allergen,
            data.severity,
            data.description
        );
    }

    toRequest() {
        return {
            allergen: this.allergen,
            severity: this.severity,
            description: this.description
        };
    }
}