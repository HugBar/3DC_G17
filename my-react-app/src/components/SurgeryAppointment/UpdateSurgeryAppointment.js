import React, {useState} from 'react';
import {useNavigate} from 'react-router-dom';
import appointmentService from '../../api/Appointment/appointmentService';
import './UpdateSurgeryAppointment.css';

const UpdateSurgeryAppointment = () => {
    const navigate = useNavigate();
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState('');
    const [success, setSuccess] = useState('');
    const [fieldErrors, setFieldErrors] = useState({});
    const [operationRequestId, setOperationRequestId] = useState('');
    const [showUpdateForm, setShowUpdateForm] = useState(false);
    
    const [appointmentData, setAppointmentData] = useState({
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

    const handleFetchAppointment = async (e) => {
        e.preventDefault();
        setError('');
        setLoading(true);
        
        try {
            const appointment = await appointmentService.getAppointmentByOperationRequestId(operationRequestId);
            const formattedAppointment = {
                ...appointment,
                scheduledDateTime: appointment.scheduledDateTime 
                    ? new Date(appointment.scheduledDateTime).toISOString().slice(0, 16)
                    : ''
            };
            setAppointmentData(formattedAppointment);
            setShowUpdateForm(true);
        } catch (err) {
            setError(err.response?.data?.message || 'Failed to fetch appointment');
            setShowUpdateForm(false);
        } finally {
            setLoading(false);
        }
    };

    const handleInputChange = (e) => {
        const { name, value } = e.target;
        setAppointmentData(prev => ({
            ...prev,
            [name]: value
        }));
    };

    const handleStaffChange = (index, value) => {
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

    const validateForm = () => {
        const errors = {};
        
        if (!appointmentData.surgeryRoomId) {
            errors.surgeryRoomId = 'Surgery Room ID is required';
        }
        if (!appointmentData.scheduledDateTime) {
            errors.scheduledDateTime = 'Scheduled date and time is required';
        }
        if (!appointmentData.estimatedDuration || appointmentData.estimatedDuration < 1) {
            errors.estimatedDuration = 'Valid estimated duration is required';
        }

        appointmentData.staffAssignments.forEach((staff, index) => {
            if (!staff.licenseNumber || !/^LIC-\d{8}$/.test(staff.licenseNumber)) {
                errors[`staffLicense${index}`] = 'Invalid license number format (LIC-XXXXXXXX)';
            }
        });

        setFieldErrors(errors);
        return Object.keys(errors).length === 0;
    };

    const handleSubmit = async (e) => {
        e.preventDefault();
        if (!validateForm()) return;

        setLoading(true);
        try {
            await appointmentService.updateSurgeryAppointment(operationRequestId, appointmentData);
            setSuccess('Surgery appointment updated successfully');
            // Reset form
            setOperationRequestId('');
            setShowUpdateForm(false);
            setAppointmentData({
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
        } catch (err) {
            setError(err.response?.data?.message || 'Failed to update appointment');
        } finally {
            setLoading(false);
        }
    };

    return (
        <div className="update-surgery-container">
            <div className="update-surgery-card">
                <h2>Update Surgery Appointment</h2>
                
                {error && <div className="error-message">{error}</div>}
                {success && <div className="success-message">{success}</div>}

                {/* Operation Request ID Form */}
                {!showUpdateForm && (
                    <form onSubmit={handleFetchAppointment} className="operation-request-form">
                        <div className="form-group">
                            <label>Operation Request ID</label>
                            <input
                                type="text"
                                value={operationRequestId}
                                onChange={(e) => setOperationRequestId(e.target.value)}
                                placeholder="Enter Operation Request ID (e.g., OP-XXXXXXXX)"
                                pattern="OP-\d{8}"
                                required
                            />
                        </div>
                        <button type="submit" disabled={loading}>
                            {loading ? 'Fetching...' : 'Find Appointment'}
                        </button>
                    </form>
                )}

                {/* Update Form */}
                {showUpdateForm && (
                    <form onSubmit={handleSubmit} className="update-surgery-form">
                        <div className="form-group">
                            <label>Surgery Room ID</label>
                            <input
                                type="text"
                                name="surgeryRoomId"
                                value={appointmentData.surgeryRoomId}
                                onChange={handleInputChange}
                                placeholder="Surgery Room ID"
                                required
                            />
                            {fieldErrors.surgeryRoomId && (
                                <div className="field-error">{fieldErrors.surgeryRoomId}</div>
                            )}
                        </div>

                        <div className="form-group">
                            <label>Scheduled Date and Time</label>
                            <input
                                type="datetime-local"
                                name="scheduledDateTime"
                                value={appointmentData.scheduledDateTime}
                                onChange={handleInputChange}
                                required
                            />
                            {fieldErrors.scheduledDateTime && (
                                <div className="field-error">{fieldErrors.scheduledDateTime}</div>
                            )}
                        </div>

                        <div className="form-group">
                            <label>Estimated Duration (minutes)</label>
                            <input
                                type="number"
                                name="estimatedDuration"
                                value={appointmentData.estimatedDuration}
                                onChange={handleInputChange}
                                min="1"
                                required
                            />
                            {fieldErrors.estimatedDuration && (
                                <div className="field-error">{fieldErrors.estimatedDuration}</div>
                            )}
                        </div>

                        <div className="form-group">
                            <label>Staff Assignments</label>
                            {appointmentData.staffAssignments.map((staff, index) => (
                                <div key={index} className="form-group">
                                    <label>Staff {index + 1} License Number</label>
                                    <input
                                        type="text"
                                        name={`staffLicense${index}`}
                                        value={staff.licenseNumber}
                                        onChange={(e) => handleStaffChange(index, e.target.value)}
                                        placeholder="LIC-XXXXXXXX"
                                        pattern="LIC-\d{8}"
                                        title="License number format: LIC-XXXXXXXX"
                                        required
                                    />
                                    {fieldErrors[`staffLicense${index}`] && (
                                        <div className="field-error">{fieldErrors[`staffLicense${index}`]}</div>
                                    )}
                                </div>
                            ))}
                        </div>

                        <div className="form-group">
                            <label>Description</label>
                            <textarea
                                name="description"
                                value={appointmentData.description}
                                onChange={handleInputChange}
                                placeholder="Description"
                            />
                            {fieldErrors.description && (
                                <div className="field-error">{fieldErrors.description}</div>
                            )}
                        </div>

                        <div className="button-group">
                            <button 
                                type="button" 
                                className="cancel-button" 
                                onClick={() => navigate('/surgery-appointments/update')}
                            >
                                Cancel
                            </button>
                            <button 
                                type="submit" 
                                className="submit-button"
                                disabled={loading}
                            >
                                {loading ? 'Updating...' : 'Update Appointment'}
                            </button>
                        </div>
                    </form>
                )}
            </div>
        </div>
    );
};

export default UpdateSurgeryAppointment;