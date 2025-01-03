import React, { useState, useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import medicalConditionService from '../../../api/MedicalCondition/medicalConditionService';
import './UpdateMedicalCondition.css';

const UpdateMedicalCondition = () => {
    const { id } = useParams();
    const navigate = useNavigate();
    const [condition, setCondition] = useState({
        name: '',
        severity: '',
        description: ''
    });
    const [successMessage, setSuccessMessage] = useState('');
    const [errorMessage, setErrorMessage] = useState('');

    // Add handleChange function
    const handleChange = (e) => {
        const { name, value } = e.target;
        setCondition(prev => ({
            ...prev,
            [name]: value
        }));
    };

    useEffect(() => {
        const fetchCondition = async () => {
            try {
                // Use getMedicalCondition instead of searchMedicalConditions
                const condition = await medicalConditionService.getMedicalCondition(id);
                if (condition) {
                    setCondition({
                        name: condition.name || '',
                        severity: condition.severity || '',
                        description: condition.description || ''
                    });
                }
            } catch (error) {
                setErrorMessage('Error fetching condition details');
            }
        };

        if (id) {
            fetchCondition();
        }
    }, [id]);

    const handleSubmit = async (e) => {
        e.preventDefault();
        try {
            await medicalConditionService.updateMedicalCondition(id, condition);
            setSuccessMessage('Medical condition updated successfully!');
            setTimeout(() => {
                navigate('/medical-conditions/search');
            }, 1000);
        } catch (error) {
            setErrorMessage('Error updating medical condition');
        }
    };

    return (
        <div className="update-medical-condition">
            <h1>Update Medical Condition</h1>
            <form onSubmit={handleSubmit}>
                <div className="form-group">
                    <label htmlFor="name">Name</label>
                    <input
                        type="text"
                        id="name"
                        name="name"
                        value={condition.name}
                        onChange={handleChange}
                    />
                </div>
                <div className="form-group">
                    <label htmlFor="severity">Severity</label>
                    <select
                        id="severity"
                        name="severity"
                        value={condition.severity}
                        onChange={handleChange}
                    >
                        <option value="Low">Low</option>
                        <option value="Medium">Medium</option>
                        <option value="High">High</option>
                    </select>
                </div>
                <div className="form-group">
                    <label htmlFor="description">Description</label>
                    <textarea
                        id="description"
                        name="description"
                        value={condition.description}
                        onChange={handleChange}
                    />
                </div>
                <button type="submit">Update Medical Condition</button>
            </form>
            {successMessage && <p className="success-message">{successMessage}</p>}
            {errorMessage && <p className="error-message">{errorMessage}</p>}
        </div>
    );
};

export default UpdateMedicalCondition;