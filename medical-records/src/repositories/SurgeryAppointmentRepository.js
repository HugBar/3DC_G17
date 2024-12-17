const SurgeryAppointment = require('../models/SurgeryAppointment');

class SurgeryAppointmentRepository {
    constructor() {
        if (SurgeryAppointmentRepository.instance) {
            return SurgeryAppointmentRepository.instance;
        }
        SurgeryAppointmentRepository.instance = this;
    }

    async create(appointmentData) {
        try {
            const appointment = new SurgeryAppointment(appointmentData);
            return await appointment.save();
        } catch (error) {
            throw error;
        }
    }

    async findById(id) {
        try {
            return await SurgeryAppointment.findById(id);
        } catch (error) {
            throw error;
        }
    }

    async findByDoctorId(doctorId) {
        try {
            return await SurgeryAppointment.find({ doctorId });
        } catch (error) {
            throw error;
        }
    }

    async checkRoomAvailability(roomId, startTime, endTime) {
        try {
            const conflictingAppointments = await SurgeryAppointment.find({
                surgeryRoomId: roomId,
                status: { $ne: 'CANCELLED' },
                $or: [
                    {
                        scheduledDateTime: { $lt: endTime },
                        endDateTime: { $gt: startTime }
                    }
                ]
            });
            return conflictingAppointments.length === 0;
        } catch (error) {
            throw error;
        }
    }

    async updateStatus(id, status) {
        try {
            return await SurgeryAppointment.findByIdAndUpdate(
                id,
                { status },
                { new: true }
            );
        } catch (error) {
            throw error;
        }
    }

    async search(filters) {
        try {
            const query = {};
            
            if (filters.doctorId) query.doctorId = filters.doctorId;
            if (filters.patientId) query.patientId = filters.patientId;
            if (filters.status) query.status = filters.status;
            if (filters.startDate || filters.endDate) {
                query.scheduledDateTime = {};
                if (filters.startDate) query.scheduledDateTime.$gte = new Date(filters.startDate);
                if (filters.endDate) query.scheduledDateTime.$lte = new Date(filters.endDate);
            }

            return await SurgeryAppointment.find(query);
        } catch (error) {
            throw error;
        }
    }
}

const surgeryAppointmentRepository = new SurgeryAppointmentRepository();
module.exports = surgeryAppointmentRepository;