class SurgeryAppointmentDto {
    constructor(data) {
        this.operationRequestId = data.operationRequestId;
        this.surgeryRoomId = data.surgeryRoomId;
        this.scheduledDateTime = data.scheduledDateTime;
        this.estimatedDuration = data.estimatedDuration;
        this.staffAssignments = data.staffAssignments.map(staff => ({
            licenseNumber: staff.licenseNumber,
            role: staff.role
        }));
        this.description = data.description;
    }

    validate() {
        if (!this.operationRequestId) throw new Error('Missing operationRequestId');
        if (!this.surgeryRoomId) throw new Error('Missing surgeryRoomId');
        if (!this.scheduledDateTime) throw new Error('Missing scheduledDateTime');
        if (!this.estimatedDuration) throw new Error('Missing estimatedDuration');
        
        if (!Array.isArray(this.staffAssignments) || this.staffAssignments.length === 0) {
            throw new Error('Staff assignments must be a non-empty array');
        }

        this.staffAssignments.forEach(staff => {
            if (!staff.licenseNumber) throw new Error('Missing staff license number');
            if (!staff.role) throw new Error('Missing staff role');
            if (!staff.licenseNumber.match(/^LIC-\d{5}$/)) {
                throw new Error('Invalid license number format. Must be LIC-XXXXX');
            }
        });
    }

    toResponse() {
        return {
            operationRequestId: this.operationRequestId,
            surgeryRoomId: this.surgeryRoomId,
            scheduledDateTime: this.scheduledDateTime,
            estimatedDuration: this.estimatedDuration,
            staffAssignments: this.staffAssignments,
            description: this.description
        };
    }
}

module.exports = SurgeryAppointmentDto;