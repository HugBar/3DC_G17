export class MedicalConditionDTO {
    constructor(name, severity, description) {
        this.name = name;
        this.severity = severity;
        this.description = description;
    }

    static fromResponse(data) {
        return new MedicalConditionDTO(
            data.name || '',
            data.severity || '',
            data.description || ''
        );
    }

    toRequest() {
        return {
            name: this.name,
            severity: this.severity,
            description: this.description
        };
    }
}