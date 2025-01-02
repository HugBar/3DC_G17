import React, { useState, useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import allergyService from '../../../api/Allergy/allergyService';
import './UpdateAllergy.css';

const UpdateAllergy = () => {
    const { id } = useParams();
    const navigate = useNavigate();
    const [allergyData, setAllergyData] = useState({
        allergen: '',
        severity: '',
        description: ''
    });
    const [successMessage, setSuccessMessage] = useState('');
    const [errorMessage, setErrorMessage] = useState('');


    useEffect(() => {
        const fetchAllergy = async () => {
            try {
                const response = await allergyService.searchAllergies();
                if (response && response.length > 0) {
                    const allergy = response.find(allergy => allergy.id === id);
                    if (allergy) {
                        setAllergyData({
                            id: allergy.id || '',
                            allergen: allergy.allergen || '',
                            severity: allergy.severity || '',
                            description: allergy.description || ''
                        });
                    } else {
                        setErrorMessage('Allergy not found');
                    }
                } else {
                    setErrorMessage('Allergy not found');
                }
            } catch (error) {
                setErrorMessage('Error fetching allergy details');
            }
        };

        fetchAllergy();
    }, [id]);


    const handleChange = (e) => {
        const { name, value } = e.target;
        setAllergyData(prevData => ({
            ...prevData,
            [name]: value
        }));
    };

    const handleSubmit = async (e) => {
        e.preventDefault();
        try {
            const searchResult = await allergyService.searchAllergies();
            if (searchResult && searchResult.length > 0) {
                const allergy = searchResult.find(allergy => allergy.id === id);
                if (!allergy.id) {
                    setErrorMessage('Error: Allergy ID not found');
                    return;
                }
                
                await allergyService.updateAllergy(allergy.id, allergyData);
                setSuccessMessage('Allergy updated successfully!');
                setErrorMessage('');
                setTimeout(() => {
                    navigate('/allergies/search');
                }, 1000);
            } else {
                setErrorMessage('Error: Allergy not found');
            }
        } catch (error) {
            setErrorMessage('Error updating allergy');
        }
    };

    return (
        <div className="update-allergy">
            <h1>Update Allergy</h1>
            <form onSubmit={handleSubmit}>
                <div className="form-group">
                    <label htmlFor="allergen">Allergen</label>
                    <input
                        type="text"
                        id="allergen"
                        name="allergen"
                        value={allergyData.allergen}
                        onChange={handleChange}
                    />
                </div>
                <div className="form-group">
                    <label htmlFor="severity">Severity</label>
                    <input
                        type="text"
                        id="severity"
                        name="severity"
                        value={allergyData.severity}
                        onChange={handleChange}
                    />
                </div>
                <div className="form-group">
                    <label htmlFor="description">Description</label>
                    <input
                        type="text"
                        id="description"
                        name="description"
                        value={allergyData.description}
                        onChange={handleChange}
                    />
                </div>
                <button type="submit">Update Allergy</button>
            </form>
            {successMessage && <p className="success-message">{successMessage}</p>}
            {errorMessage && <p className="error-message">{errorMessage}</p>}
        </div>
    );


};

export default UpdateAllergy;