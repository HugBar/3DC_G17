import React, { useState } from 'react';
import medicalConditionService from '../../../api/medicalConditionService';
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
            await medicalConditionService.addMedicalCondition(medicalConditionData);
            setSuccessMessage('Condição médica adicionada com sucesso!');
            setErrorMessage('');
            // Limpar formulário
            setMedicalConditionData({
                name: '',
                severity: 'Low',
                description: ''
            });
        } catch (error) {
            setErrorMessage(error.response?.data?.message || 'Erro ao adicionar condição médica');
            setSuccessMessage('');
        }
    };

    return (
        <div className="add-medical-condition-container">
            <h2>Adicionar Nova Condição Médica</h2>
            {successMessage && <p className="success-message">{successMessage}</p>}
            {errorMessage && <p className="error-message">{errorMessage}</p>}
            
            <form onSubmit={handleSubmit} className="medical-condition-form">
                <div className="form-group">
                    <label htmlFor="name">Nome da Condição:</label>
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
                    <label htmlFor="severity">Severidade:</label>
                    <select
                        id="severity"
                        name="severity"
                        value={medicalConditionData.severity}
                        onChange={handleChange}
                        required
                    >
                        <option value="Low">Baixa</option>
                        <option value="Medium">Média</option>
                        <option value="High">Alta</option>
                    </select>
                </div>

                <div className="form-group">
                    <label htmlFor="description">Descrição:</label>
                    <textarea
                        id="description"
                        name="description"
                        value={medicalConditionData.description}
                        onChange={handleChange}
                        required
                    />
                </div>

                <button type="submit" className="submit-button">
                    Adicionar Condição Médica
                </button>
            </form>
        </div>
    );
};

export default AddMedicalCondition;
