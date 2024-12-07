import React, { useState } from 'react';
import allergyService from '../../api/allergyService';
import './Allergy.css';

const AddAllergy = () => {
    const [allergyData, setAllergyData] = useState({
        allergen: '',
        severity: 'Low',
        description: ''
    });

    const [message, setMessage] = useState({ text: '', type: '' });

    const handleChange = (e) => {
        const { name, value } = e.target;
        setAllergyData(prev => ({
            ...prev,
            [name]: value
        }));
    };

    const handleSubmit = async (e) => {
        e.preventDefault();
        try {
            await allergyService.addAllergy(allergyData);
            setMessage({ text: 'Allergy added successfully', type: 'success' });
            // Clear form
            setAllergyData({
                allergen: '',
                severity: 'Low',
                description: ''
            });

        } catch (error) {
            setMessage({ 
                text: error.response?.data?.message || 'Error adding allergy', 
                type: 'error' 
            });
        }
    };

    return (
        <div className="allergy-container">
            <h2>Add New Allergy</h2>
            
            {message.text && (
                <div className={`message ${message.type}`}>
                    {message.text}
                </div>
            )}

            <form onSubmit={handleSubmit} className="allergy-form">
                <div className="form-group">
                    <label htmlFor="allergen">Allergen:</label>
                    <input
                        type="text"
                        id="allergen"
                        name="allergen"
                        value={allergyData.allergen}
                        onChange={handleChange}
                        required
                    />
                </div>

                <div className="form-group">
                    <label htmlFor="severity">Severity:</label>
                    <select
                        id="severity"
                        name="severity"
                        value={allergyData.severity}
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
                        value={allergyData.description}
                        onChange={handleChange}
                        required
                    />
                </div>

                <button type="submit" className="submit-button">
                    Add Allergy
                </button>
            </form>
        </div>
    );
};

export default AddAllergy;
