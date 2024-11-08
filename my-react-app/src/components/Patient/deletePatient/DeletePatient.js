import React, { useState } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import patientService from '../../../api/patientService';
import './DeletePatient.css';

const DeletePatient = () => {
  const navigate = useNavigate();
  const { id: patientId } = useParams();
  const [successMessage, setSuccessMessage] = useState('');
  const [isDeleted, setIsDeleted] = useState(false); // Novo estado para controlar a exclusão

  const handleDelete = async () => {
    try {
      await patientService.deletePatient(patientId);
      setSuccessMessage('Patient successfully deleted.');
      setIsDeleted(true); // Atualiza o estado para indicar que o paciente foi excluído
      setTimeout(() => navigate('/patient/list'), 3000); // Redireciona após 3 segundos
    } catch (error) {
      console.error('Error deleting patient:', error);
      alert('Error deleting patient.');
    }
  };

  return (
    <div className="delete-patient-container">
      <div className="modal">
        <div className="modal-content">
          {isDeleted ? ( // Verifica se o paciente foi excluído
            <div className="success-message">
              {successMessage}
            </div>
          ) : (
            <>
              <h3>Confirm Deletion</h3>
              <p>Are you sure you want to delete this patient?</p>
              <button onClick={handleDelete} className="confirm-button">Yes</button>
              <button onClick={() => navigate(`/patient/list`)} className="cancel-button">No</button>
            </>
          )}
        </div>
      </div>
    </div>
  );
};

export default DeletePatient;