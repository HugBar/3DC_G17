// Author: Pedro Azevedo

/**
 * Data Transfer Object for surgery appointments
 * Used to transfer surgery appointment data between layers of the application
 * Contains information about the operation, room, schedule, staff and other details
 */
class SurgeryAppointmentDto {
    /**
     * Creates a new SurgeryAppointmentDto instance
     * @param {Object} data - The surgery appointment data
     * @param {string} data.operationRequestId - ID of the operation request
     * @param {string} data.surgeryRoomId - ID of the surgery room
     * @param {Date} data.scheduledDateTime - Scheduled date and time
     * @param {number} data.estimatedDuration - Estimated duration in minutes
     * @param {Array} data.staffAssignments - Array of assigned staff members
     * @param {string} data.description - Description of the surgery
     */
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

    /**
     * Validates the surgery appointment data
     * Checks for required fields and proper formats
     * @throws {Error} If any validation fails
     */
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

    /**
     * Converts the DTO to a response object
     * @returns {Object} The surgery appointment data in response format
     */
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