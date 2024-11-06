// src/components/OperationRequestDeleteConfirmation/OperationRequestDeleteConfirmation.js
import React from 'react';
import operationRequestService from '../../api/operationRequestService'; // Import the service
import './OperationRequestDeleteConfirmation.css';

const OperationRequestDeleteConfirmation = ({ operationRequestId, onConfirm, onCancel }) => {
  const handleConfirmDelete = async () => {
    try {
      // Call the delete function and pass the ID
      await operationRequestService.deleteOperationRequest(operationRequestId);
      onConfirm(); // Call onConfirm to update the parent component after deletion
    } catch (error) {
      console.error("Error deleting operation request:", error);
      alert("An error occurred while deleting the operation request. Please try again.");
    }
  };

  return (
    <div className="delete-confirmation-overlay">
      <div className="delete-confirmation-modal">
        <h3>Confirm Deletion</h3>
        <p>Are you sure you want to delete this operation request? </p>
        <button onClick={handleConfirmDelete} className="confirm-button">Yes, Delete</button>
        <button onClick={onCancel} className="cancel-button">Cancel</button>
      </div>
    </div>
  );
};

export default OperationRequestDeleteConfirmation;
