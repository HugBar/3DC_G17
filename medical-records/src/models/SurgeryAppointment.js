const mongoose = require('mongoose');

const surgeryAppointmentSchema = new mongoose.Schema({
    operationRequestId: {
        type: mongoose.Schema.Types.ObjectId,
        required: true,
        ref: 'OperationRequest'
    },
    scheduledDateTime: {
        type: Date,
        required: true
    },
    estimatedDuration: {
        type: Number,
        required: true,
        min: 0
    },
    surgeryRoomId: {
        type: String,
        required: true
    },
    staffAssignments: [{
        staffId: {
            type: String,
            required: true
        },
        role: {
            type: String,
            required: true,
            enum: ['SURGEON', 'NURSE', 'ANESTHESIOLOGIST']
        }
    }],
    status: {
        type: String,
        required: true,
        enum: ['SCHEDULED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED'],
        default: 'SCHEDULED'
    },
    endDateTime: {
        type: Date,
        required: true
    }
}, {
    timestamps: true
});

// Calculate endDateTime before saving
surgeryAppointmentSchema.pre('save', function(next) {
    if (this.scheduledDateTime && this.estimatedDuration) {
        this.endDateTime = new Date(this.scheduledDateTime.getTime() + this.estimatedDuration * 60000);
    }
    next();
});

module.exports = mongoose.model('SurgeryAppointment', surgeryAppointmentSchema);