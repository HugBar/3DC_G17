// Author: Matias Vitorino

/**
 * This component allows Admins to remove a specialization from the system.
 * It displays a confirmation message and handles the deletion process.
 * - The user can confirm or cancel the deletion.
 * - On successful deletion, a success message is displayed.
 * - On error, an error message is displayed.
 * - The user is redirected to the search page after 3 seconds.
 */

import React, { useState, useEffect } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import specializationService from '../../../api/specializationService';
import './RemoveSpecialization.css';

const RemoveSpecialization = () => {
    const navigate = useNavigate();
    const { id: specializationId } = useParams();
    const [successMessage, setSuccessMessage] = useState('');
    const [isDeleted, setIsDeleted] = useState(false);
    const [error, setError] = useState('');

    useEffect(() => {
        // Validate ID on component mount
        if (!specializationId || specializationId === 'undefined') {
            console.error('Invalid specialization ID:', specializationId);
            setError('Invalid specialization ID');
            setTimeout(() => navigate('/specializations/search'), 2000);
        }
    }, [specializationId, navigate]);

    const handleDelete = async () => {
        try {
            if (!specializationId || specializationId === 'undefined') {
                throw new Error('Invalid specialization ID');
            }
            console.log('Attempting to delete specialization with ID:', specializationId);
            
            const result = await specializationService.deleteSpecialization(specializationId);
            console.log('Delete result:', result);
            
            setSuccessMessage('Specialization successfully deleted.');
            setIsDeleted(true);
            setTimeout(() => navigate('/specializations/search'), 3000);
        } catch (error) {
            console.error('Error deleting specialization:', error);
            alert('Error deleting specialization.');
        }
    };

    if (error) {
        return (
            <div className="delete-specialization-container">
                <div className="error-message">{error}</div>
            </div>
        );
    }

    return (
        <div className="delete-specialization-container">
            <div className="modal-content">
                {isDeleted ? (
                    <div className="success-message">
                        {successMessage}
                    </div>
                ) : (
                    <>
                        <h3>Delete Specialization</h3>
                        <p>Are you sure you want to delete this specialization?</p>
                        <div className="modal-actions">
                            <button onClick={handleDelete} className="confirm-button">
                                Yes, Delete
                            </button>
                            <button onClick={() => navigate('/specializations/search')} className="cancel-button">
                                Cancel
                            </button>
                        </div>
                    </>
                )}
            </div>
        </div>
    );
};

export default RemoveSpecialization;