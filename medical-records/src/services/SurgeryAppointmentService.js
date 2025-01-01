const surgeryAppointmentRepository = require('../repositories/SurgeryAppointmentRepository');

class SurgeryAppointmentService {
    static async createSurgeryAppointment(appointmentDto) {
        try {


        
            const appointmentData = {
                operationRequestId: appointmentDto.operationRequestId,
                surgeryRoomId: appointmentDto.surgeryRoomId,
                scheduledDateTime: appointmentDto.scheduledDateTime,
                estimatedDuration: appointmentDto.estimatedDuration,
                staffAssignments: appointmentDto.staffAssignments,
                description: appointmentDto.description,
                status: 'SCHEDULED'
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

    static async validateOperationRequest(appointmentId, operationRequestId) {
        try {
            const appointment = await surgeryAppointmentRepository.findById(appointmentId);
            
            if (!appointment) {
                throw new Error('Appointment not found');
            }

            if (appointment.operationRequestId !== operationRequestId) {
                throw new Error('Invalid operation request ID');
            }

            return appointment;
        } catch (error) {
            throw error;
        }
    }

    static async updateSurgeryAppointment(operationRequestId, updateData) {
        try {            
            return await surgeryAppointmentRepository.update(operationRequestId, updateData);
        } catch (error) {
            throw error;
        }
    }

    static async findByOperationRequestId(operationRequestId) {
        try {
            return await surgeryAppointmentRepository.findByOperationRequestId(operationRequestId);
        } catch (error) {
            throw error;
        }
    }
}

module.exports = SurgeryAppointmentService;