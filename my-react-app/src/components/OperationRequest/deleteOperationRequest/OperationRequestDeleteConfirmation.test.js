import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import OperationRequestDeleteConfirmation from './OperationRequestDeleteConfirmation';
import operationRequestService from '../../../api/operationRequestService';

// Mock the service
jest.mock('../../../api/operationRequestService');

describe('OperationRequestDeleteConfirmation Component', () => {
  const mockOnConfirm = jest.fn();
  const mockOnCancel = jest.fn();
  const mockOperationRequestId = '123';

  beforeEach(() => {
    jest.clearAllMocks();
  });

  test('renders confirmation modal with correct text', () => {
    render(
      <OperationRequestDeleteConfirmation
        operationRequestId={mockOperationRequestId}
        onConfirm={mockOnConfirm}
        onCancel={mockOnCancel}
      />
    );

    expect(screen.getByText('Confirm Deletion')).toBeInTheDocument();
    expect(screen.getByText('Are you sure you want to delete this operation request?')).toBeInTheDocument();
    expect(screen.getByText('Yes, Delete')).toBeInTheDocument();
    expect(screen.getByText('Cancel')).toBeInTheDocument();
  });

  test('calls onCancel when cancel button is clicked', () => {
    render(
      <OperationRequestDeleteConfirmation
        operationRequestId={mockOperationRequestId}
        onConfirm={mockOnConfirm}
        onCancel={mockOnCancel}
      />
    );

    fireEvent.click(screen.getByText('Cancel'));
    expect(mockOnCancel).toHaveBeenCalledTimes(1);
  });

  test('calls onConfirm after successful deletion', async () => {
    operationRequestService.deleteOperationRequest.mockResolvedValueOnce();

    render(
      <OperationRequestDeleteConfirmation
        operationRequestId={mockOperationRequestId}
        onConfirm={mockOnConfirm}
        onCancel={mockOnCancel}
      />
    );

    fireEvent.click(screen.getByText('Yes, Delete'));
    
    await waitFor(() => {
      expect(operationRequestService.deleteOperationRequest).toHaveBeenCalledWith(mockOperationRequestId);
      expect(mockOnConfirm).toHaveBeenCalledTimes(1);
    });
  });

  test('shows error message when deletion fails', async () => {
    const alertMock = jest.spyOn(window, 'alert').mockImplementation(() => {});
    const consoleErrorSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
    operationRequestService.deleteOperationRequest.mockRejectedValueOnce(new Error('Delete failed'));

    render(
      <OperationRequestDeleteConfirmation
        operationRequestId={mockOperationRequestId}
        onConfirm={mockOnConfirm}
        onCancel={mockOnCancel}
      />
    );

    fireEvent.click(screen.getByText('Yes, Delete'));
    
    await waitFor(() => {
      expect(alertMock).toHaveBeenCalledWith('An error occurred while deleting the operation request. Please try again.');
      expect(mockOnConfirm).not.toHaveBeenCalled();
    });
    
    alertMock.mockRestore();
    consoleErrorSpy.mockRestore();
  });
}); 