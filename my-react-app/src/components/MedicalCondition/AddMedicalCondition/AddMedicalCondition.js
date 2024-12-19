//Author: Matias Vitorino

/**
 * This component allows Admins to add a new medical condition to the system.
 * It includes form fields for the condition name, severity, and description.
 *  - The name field is required and must be unique.
 * - The severity field is a dropdown with Low, Medium, and High options.
 * - The description field is optional and allows for additional details.
 * - The form submission triggers a POST request to the backend to add the new condition.
 * - Success and error messages are displayed based on the response from the server.
 * - The form is cleared after a successful submission.S
 */

import React, { useState } from 'react';
import medicalConditionService from '../../../api/MedicalCondition/medicalConditionService';
import { MedicalConditionDTO } from '../../../dtos/MedicalConditionDTO';
import './AddMedicalCondition.css';

const AddMedicalCondition = () => {
    const [medicalConditionData, setMedicalConditionData] = useState({
        name: '',
        severity: 'Low',
        description: ''
    });
    const [successMessage, setSuccessMessage] = useState('');
    const [errorMessage, setErrorMessage] = useState('');

    const handleChange = (e) => {
        const { name, value } = e.target;
        setMedicalConditionData(prevData => ({
            ...prevData,
            [name]: value
        }));
    };

    const handleSubmit = async (e) => {
        e.preventDefault();
        try {
            const medicalConditionDto = new MedicalConditionDTO(
                medicalConditionData.name,
                medicalConditionData.severity,
                medicalConditionData.description
            );
            
            await medicalConditionService.addMedicalCondition(medicalConditionDto.toRequest());
            setSuccessMessage('Medical condition added successfully!');
            setErrorMessage('');
            // Clear form
            setMedicalConditionData({
                name: '',
                severity: 'Low',
                description: ''
            });
        } catch (error) {
            setErrorMessage(error.response?.data?.message || 'Error adding medical condition');
            setSuccessMessage('');
        }
    };

    return (
        <div className="add-medical-condition-container">
            <h2>Add New Medical Condition</h2>
            {successMessage && <p className="success-message">{successMessage}</p>}
            {errorMessage && <p className="error-message">{errorMessage}</p>}
            
            <form onSubmit={handleSubmit} className="medical-condition-form">
                <div className="form-group">
                    <label htmlFor="name">Condition Name:</label>
                    <input
                        id="name"
                        name="name"
                        type="text"
                        value={medicalConditionData.name}
                        onChange={handleChange}
                        required
                    />
                </div>

                <div className="form-group">
                    <label htmlFor="severity">Severity:</label>
                    <select
                        id="severity"
                        name="severity"
                        value={medicalConditionData.severity}
                        onChange={handleChange}
                        required
                    >
                        <option value="Low">Low</option>
                        <option value="Medium">Medium</option>
                        <option value="High">High</option>
                    </select>
                </div>

                <div className="form-group">
                    <label htmlFor="description">Description:</label>
                    <textarea
                        id="description"
                        name="description"
                        value={medicalConditionData.description}
                        onChange={handleChange}
                        required
                    />
                </div>

                <button type="submit" className="submit-button">
                    Add Medical Condition
                </button>
            </form>
        </div>
    );
};

export default AddMedicalCondition;