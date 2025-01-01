const mongoose = require('mongoose');

const surgeryAppointmentSchema = new mongoose.Schema({
    operationRequestId: {
        type: String,
        required: true
    },
    surgeryRoomId: {
        type: String,
        required: true
    },
    scheduledDateTime: {
        type: Date,
        required: true
    },
    estimatedDuration: {
        type: Number,
        required: true
    },
    staffAssignments: [{
        licenseNumber: {
            type: String,
            required: true,
            match: /^LIC-\d{8}$/
        },
        role: {
            type: String,
            required: true,
            enum: ['DOCTOR', 'NURSE','TECHNICIAN']
        }
    }],
    description: String
});


module.exports = mongoose.model('SurgeryAppointment', surgeryAppointmentSchema);