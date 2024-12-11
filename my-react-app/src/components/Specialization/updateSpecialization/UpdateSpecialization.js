import React, { useState, useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import specializationService from '../../../api/specializationService';
import './UpdateSpecialization.css';

const UpdateSpecialization = () => {
    const { id: name } = useParams();
    const navigate = useNavigate();
    const [specializationData, setSpecializationData] = useState({
        name: '',
        description: ''
    });
    const [successMessage, setSuccessMessage] = useState('');
    const [errorMessage, setErrorMessage] = useState('');

    useEffect(() => {
        const fetchSpecialization = async () => {
            try {
                const decodedName = decodeURIComponent(name);
                const response = await specializationService.searchSpecializations({ name: decodedName });
                if (response && response.length > 0) {
                    const specialization = response[0];
                    setSpecializationData({
                        name: specialization.name,
                        description: specialization.description
                    });
                } else {
                    setErrorMessage('Specialization not found');
                }
            } catch (error) {
                setErrorMessage('Error fetching specialization details');
            }
        };

        fetchSpecialization();
    }, [name]);

    const handleChange = (e) => {
        const { name, value } = e.target;
        setSpecializationData(prevData => ({
            ...prevData,
            [name]: value
        }));
    };

    const handleSubmit = async (e) => {
        e.preventDefault();
        try {
            const searchResult = await specializationService.searchSpecializations({ name: decodeURIComponent(name) });
            if (searchResult && searchResult.length > 0) {
                const specialization = searchResult[0];
                if (!specialization._id) {
                    setErrorMessage('Error: Specialization ID not found');
                    return;
                }
                
                await specializationService.updateSpecialization(specialization._id, specializationData);
                setSuccessMessage('Specialization updated successfully!');
                setErrorMessage('');
                setTimeout(() => {
                    navigate('/specializations/search');
                }, 2000);
            } else {
                setErrorMessage('Specialization not found');
            }
        } catch (error) {
            console.error('Error details:', error);
            setErrorMessage(error.response?.data?.message || 'Error updating specialization');
            setSuccessMessage('');
        }
    };

    return (
        <div className="update-specialization-container">
            <h2>Update Specialization</h2>
            {successMessage && <p className="success-message">{successMessage}</p>}
            {errorMessage && <p className="error-message">{errorMessage}</p>}
            
            <form onSubmit={handleSubmit} className="specialization-form">
                <div className="form-group">
                    <label htmlFor="name">Specialization Name:</label>
                    <input
                        id="name"
                        name="name"
                        type="text"
                        value={specializationData.name}
                        onChange={handleChange}
                        required
                    />
                </div>

                <div className="form-group">
                    <label htmlFor="description">Description:</label>
                    <textarea
                        id="description"
                        name="description"
                        value={specializationData.description}
                        onChange={handleChange}
                        required
                    />
                </div>

                <button type="submit" className="submit-button">
                    Update Specialization
                </button>
            </form>
        </div>
    );
};

export default UpdateSpecialization;