class SurgeryAppointmentDto {
    constructor(data) {
        // Validate required fields
        const requiredFields = [
            'operationRequestId',
            'surgeryRoomId',
            'scheduledDateTime',
            'estimatedDuration',
            'staffAssignments'
        ];

        for (const field of requiredFields) {
            if (!data[field]) {
                throw new Error(`Missing required field: ${field}`);
            }
        }

        // Parse and validate scheduledDateTime first
        const scheduledDate = new Date(data.scheduledDateTime);
        if (isNaN(scheduledDate.getTime())) {
            throw new Error('Invalid scheduledDateTime format. Please use ISO 8601 format (e.g., "2024-01-20T10:00:00.000Z")');
        }

        // Validate staffAssignments
        if (!Array.isArray(data.staffAssignments) || data.staffAssignments.length === 0) {
            throw new Error('Staff assignments must be a non-empty array');
        }

        const validRoles = ['SURGEON', 'NURSE', 'ANESTHESIOLOGIST'];
        for (const staff of data.staffAssignments) {
            if (!staff.staffId || !staff.role) {
                throw new Error('Each staff assignment must have staffId and role');
            }
            if (!validRoles.includes(staff.role)) {
                throw new Error(`Invalid role: ${staff.role}. Must be one of: ${validRoles.join(', ')}`);
            }
        }

        // Assign values after all validations pass
        this.operationRequestId = data.operationRequestId;
        this.surgeryRoomId = data.surgeryRoomId;
        this.scheduledDateTime = scheduledDate; // Use the already parsed date
        this.estimatedDuration = Number(data.estimatedDuration);
        this.staffAssignments = data.staffAssignments;
        this.description = data.description || '';
    }
}

module.exports = SurgeryAppointmentDto;