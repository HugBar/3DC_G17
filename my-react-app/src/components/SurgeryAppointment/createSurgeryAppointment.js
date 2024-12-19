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
            { staffId: '', role: 'SURGEON' },
            { staffId: '', role: 'NURSE' },
            { staffId: '', role: 'ANESTHESIOLOGIST' }
        ],
        description: ''
    });
    const [successMessage, setSuccessMessage] = useState('');
    const [errorMessage, setErrorMessage] = useState('');

    const handleChange = (e) => {
        const { name, value } = e.target;
        setAppointmentData(prevData => ({
            ...prevData,
            [name]: value
        }));
    };

    const handleStaffAssignmentChange = (index, field, value) => {
        setAppointmentData(prevData => {
            const newStaffAssignments = [...prevData.staffAssignments];
            newStaffAssignments[index] = {
                ...newStaffAssignments[index],
                [field]: value
            };
            return {
                ...prevData,
                staffAssignments: newStaffAssignments
            };
        });
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
                    { staffId: '', role: 'SURGEON' },
                    { staffId: '', role: 'NURSE' },
                    { staffId: '', role: 'ANESTHESIOLOGIST' }
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
                    <label htmlFor="operationRequestId">Operation Request ID:</label>
                    <input
                        id="operationRequestId"
                        name="operationRequestId"
                        type="text"
                        value={appointmentData.operationRequestId}
                        onChange={handleChange}
                        required
                    />
                </div>

                <div className="form-group">
                    <label htmlFor="surgeryRoomId">Surgery Room:</label>
                    <input
                        id="surgeryRoomId"
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

                <div className="form-group">
                    <label>Staff Assignments:</label>
                    {appointmentData.staffAssignments.map((staff, index) => (
                        <div key={index} className="staff-assignment">
                            <input
                                type="text"
                                value={staff.staffId}
                                onChange={(e) => handleStaffAssignmentChange(index, 'staffId', e.target.value)}
                                placeholder={`${staff.role} ID`}
                                required
                            />
                        </div>
                    ))}
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