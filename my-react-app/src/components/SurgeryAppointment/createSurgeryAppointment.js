import React, { useState } from 'react';
import appointmentService from '../../api/Appointment/appointmentService';
import './CreateSurgeryAppointment.css';

const CreateSurgeryAppointment = () => {
    const [appointmentData, setAppointmentData] = useState({
        operationRequestId: '',
        surgeryRoomId: '',
        scheduledDateTime: '',
        estimatedDuration: '',
        staffAssignments: [
            { licenseNumber: '', role: 'DOCTOR' },
            { licenseNumber: '', role: 'NURSE' },
            { licenseNumber: '', role: 'TECHNICIAN' }
        ],
        description: ''
    });
    const [successMessage, setSuccessMessage] = useState('');
    const [errorMessage, setErrorMessage] = useState('');

    const handleChange = (e) => {
        const { name, value } = e.target;
        setAppointmentData(prevData => ({
            ...prevData,
            [name]: name === 'estimatedDuration' ? Number(value) : value
        }));
    };

    const handleStaffAssignmentChange = (index, value) => {
        const newStaffAssignments = [...appointmentData.staffAssignments];
        newStaffAssignments[index] = {
            ...newStaffAssignments[index],
            licenseNumber: value
        };
        setAppointmentData(prev => ({
            ...prev,
            staffAssignments: newStaffAssignments
        }));
    };

    const handleSubmit = async (e) => {
        e.preventDefault();
        
        // Log the data being sent
        console.log('Sending appointment data:', appointmentData);
        
        try {
            const response = await appointmentService.createSurgeryAppointment(appointmentData);
            console.log('Success response:', response);
            setSuccessMessage('Surgery appointment created successfully!');
            setErrorMessage('');
            // Clear form
            setAppointmentData({
                operationRequestId: '',
                surgeryRoomId: '',
                scheduledDateTime: '',
                estimatedDuration: '',
                staffAssignments: [
                    { licenseNumber: '', role: 'DOCTOR' },
                    { licenseNumber: '', role: 'NURSE' },
                    { licenseNumber: '', role: 'TECHNICIAN' }
                ],
                description: ''
            });
        } catch (error) {
            console.error('Detailed error:', {
                status: error.response?.status,
                data: error.response?.data,
                message: error.message
            });
            setErrorMessage('Failed to create appointment. Please check all fields and try again.');
            setSuccessMessage('');
        }
    };

    return (
        <div className="create-surgery-appointment-container">
            <h2>Create Surgery Appointment</h2>
            {successMessage && <p className="success-message">{successMessage}</p>}
            {errorMessage && <p className="error-message">{errorMessage}</p>}
            
            <form onSubmit={handleSubmit} className="surgery-appointment-form">
                <div className="form-group">
                    <label htmlFor="operationRequest">Operation Request ID:</label>
                    <input
                        id="operationRequest"
                        name="operationRequestId"
                        type="text"
                        value={appointmentData.operationRequestId}
                        onChange={handleChange}
                        required
                    />
                </div>

                <div className="form-group">
                    <label htmlFor="surgeryRoom">Surgery Room:</label>
                    <input
                        id="surgeryRoom"
                        name="surgeryRoomId"
                        type="text"
                        value={appointmentData.surgeryRoomId}
                        onChange={handleChange}
                        required
                    />
                </div>

                <div className="form-group">
                    <label htmlFor="scheduledDateTime">Scheduled Date and Time:</label>
                    <input
                        id="scheduledDateTime"
                        name="scheduledDateTime"
                        type="datetime-local"
                        value={appointmentData.scheduledDateTime}
                        onChange={handleChange}
                        required
                    />
                </div>

                <div className="form-group">
                    <label htmlFor="estimatedDuration">Estimated Duration (minutes):</label>
                    <input
                        id="estimatedDuration"
                        name="estimatedDuration"
                        type="number"
                        min="1"
                        value={appointmentData.estimatedDuration}
                        onChange={handleChange}
                        required
                    />
                </div>

                <div className="form-section">
                    <label>Staff Assignments: *</label>
                    <div className="staff-assignments-container">
                        <input
                            type="text"
                            placeholder="DOCTOR LICENSE NUMBER"
                            value={appointmentData.staffAssignments[0].licenseNumber}
                            onChange={(e) => handleStaffAssignmentChange(0, e.target.value)}
                            pattern="LIC-\d{8}"
                            required
                        />
                        <input
                            type="text"
                            placeholder="NURSE LICENSE NUMBER"
                            value={appointmentData.staffAssignments[1].licenseNumber}
                            onChange={(e) => handleStaffAssignmentChange(1, e.target.value)}
                            pattern="LIC-\d{8}"
                            required
                        />
                        <input
                            type="text"
                            placeholder="TECHNICIAN LICENSE NUMBER"
                            value={appointmentData.staffAssignments[2].licenseNumber}
                            onChange={(e) => handleStaffAssignmentChange(2, e.target.value)}
                            pattern="LIC-\d{8}"
                            required
                        />
                    </div>
                </div>

                <div className="form-group">
                    <label htmlFor="description">Description:</label>
                    <textarea
                        id="description"
                        name="description"
                        value={appointmentData.description}
                        onChange={handleChange}
                    />
                </div>

                <button type="submit" className="submit-button">
                    Create Appointment
                </button>
            </form>
        </div>
    );
};

export default CreateSurgeryAppointment;