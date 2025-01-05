import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import patientService from '../../../api/patientService';
import './TransferRecord.css';

const TransferRecord = () => {
    const [message, setMessage] = useState('');
    const [error, setError] = useState('');
    const navigate = useNavigate();

    const handleTransferRequest = async () => {
        try {
            await patientService.requestMedicalHistoryDownload();
            setMessage('Please check your email for the medical history download link.');
            setError('');
        } catch (err) {
            setError(err.response?.data || 'Failed to request medical history transfer');
            setMessage('');
        }
    };

    return (
        <div className="transfer-record-container">
            <h2>Transfer Medical Records</h2>
            <p className="description">
                Request to download your medical history. A secure link will be sent to your email.
            </p>
            <button 
                className="transfer-button"
                onClick={handleTransferRequest}
            >
                Request Medical History
            </button>
            {message && <p className="success-message">{message}</p>}
            {error && <p className="error-message">{error}</p>}
            <button 
                className="back-button"
                onClick={() => navigate(-1)}
            >
                Back
            </button>
        </div>
    );
};

export default TransferRecord;
