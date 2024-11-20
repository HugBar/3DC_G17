// DTO for Availability Slots
export class AvailabilitySlotDTO {
  constructor(startTime, endTime) {
    this.startTime = startTime;
    this.endTime = endTime;
  }

  static fromJSON(json) {
    return new AvailabilitySlotDTO(
      json.startTime,
      json.endTime
    );
  }
}

// DTO for Creating Staff
export class CreateStaffDTO {
  constructor(firstName, lastName, email, phoneNumber, specialization, availabilitySlots) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.email = email;
    this.phoneNumber = phoneNumber;
    this.specialization = specialization;
    this.availabilitySlots = availabilitySlots.map(slot => 
      slot instanceof AvailabilitySlotDTO ? slot : new AvailabilitySlotDTO(slot.startTime, slot.endTime)
    );
  }

  static fromFormData(formData) {
    return new CreateStaffDTO(
      formData.firstName,
      formData.lastName,
      formData.email,
      formData.phoneNumber,
      formData.specialization,
      formData.availabilitySlots
    );
  }
}

// DTO for Staff Response
export class StaffDTO {
  constructor(id, firstName, lastName, email, phoneNumber, specialization, licenseNumber, active, availabilitySlots) {
    this.id = id;
    this.firstName = firstName;
    this.lastName = lastName;
    this.email = email;
    this.phoneNumber = phoneNumber;
    this.specialization = specialization;
    this.licenseNumber = licenseNumber;
    this.active = active;
    this.availabilitySlots = availabilitySlots.map(slot => 
      slot instanceof AvailabilitySlotDTO ? slot : AvailabilitySlotDTO.fromJSON(slot)
    );
  }

  static fromJSON(json) {
    return new StaffDTO(
      json.id,
      json.firstName,
      json.lastName,
      json.email,
      json.phoneNumber,
      json.specialization,
      json.licenseNumber,
      json.active,
      json.availabilitySlots
    );
  }
} 