class CreateMedicalConditionDto {
  constructor(name, severity, description) {
    this.name = name;
    this.severity = severity;
    this.description = description;
  }
}

module.exports = CreateMedicalConditionDto;