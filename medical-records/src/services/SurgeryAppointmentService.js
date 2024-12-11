const surgeryAppointmentRepository = require('../repositories/SurgeryAppointmentRepository');

class SurgeryAppointmentService {
    static async createSurgeryAppointment(appointmentDto) {
        try {
            const endDateTime = new Date(
                appointmentDto.scheduledDateTime.getTime() + 
                appointmentDto.estimatedDuration * 60000
            );

            const isRoomAvailable = await surgeryAppointmentRepository.checkRoomAvailability(
                appointmentDto.surgeryRoomId,
                appointmentDto.scheduledDateTime,
                endDateTime
            );

            if (!isRoomAvailable) {
                throw new Error('Room not available');
            }

            const appointmentData = {
                operationRequestId: appointmentDto.operationRequestId,
                surgeryRoomId: appointmentDto.surgeryRoomId,
                scheduledDateTime: appointmentDto.scheduledDateTime,
                estimatedDuration: appointmentDto.estimatedDuration,
                staffAssignments: appointmentDto.staffAssignments,
                description: appointmentDto.description,
                status: 'SCHEDULED',
                endDateTime
            };

            return await surgeryAppointmentRepository.create(appointmentData);
        } catch (error) {
            throw error;
        }
    }

    static async getDoctorAppointments(doctorId) {
        try {
            return await surgeryAppointmentRepository.findByDoctorId(doctorId);
        } catch (error) {
            throw error;
        }
    }

    static async updateAppointmentStatus(appointmentId, status) {
        try {
            const validStatuses = ['SCHEDULED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED'];
            if (!validStatuses.includes(status)) {
                throw new Error('Invalid status');
            }

            const appointment = await surgeryAppointmentRepository.findById(appointmentId);
            if (!appointment) {
                throw new Error('Appointment not found');
            }

            return await surgeryAppointmentRepository.updateStatus(appointmentId, status);
        } catch (error) {
            throw error;
        }
    }

    static async searchAppointments(searchDto) {
        try {
            return await surgeryAppointmentRepository.search(searchDto);
        } catch (error) {
            throw error;
        }
    }
}

module.exports = SurgeryAppointmentService;