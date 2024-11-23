import React from 'react';
import { render, screen, fireEvent, act, waitFor } from '@testing-library/react';
import CreateOperationRequest from './CreateOperationRequest';
import operationRequestService from '../../../api/operationRequestService';

jest.mock('../../../api/operationRequestService', () => ({
  __esModule: true,
  default: {
    createOperationRequest: jest.fn(),
    getAllOperationTypes: jest.fn(),
  },
}));

const mockOperationTypes = [
  {
    operationTypeCode: 'OT789',
    name: 'General Surgery',
    version: 1
  },
  {
    operationTypeCode: 'OT790',
    name: 'Cardiac Surgery',
    version: 2
  }
];

const mockOperationData = {
  patientMRN: 'P123',
  doctorLicenseNumber: 'LIC-12345678',
  operationTypeId: 'OT789',
  deadline: '2024-12-01T10:00',
  priority: 'urgent'
};

describe('CreateOperationRequest Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    operationRequestService.getAllOperationTypes.mockResolvedValue(mockOperationTypes);
  });

  test('renders create operation request form with operation types', async () => {
    render(<CreateOperationRequest />);
    
    await waitFor(() => {
      expect(screen.getByText('Create Operation Request')).toBeInTheDocument();
      expect(screen.getByLabelText('Patient Medical Record Number:')).toBeInTheDocument();
      expect(screen.getByLabelText('Doctor License Number:')).toBeInTheDocument();
      expect(screen.getByLabelText('Operation Type:')).toBeInTheDocument();
      
      // Check if operation types are rendered correctly
      expect(screen.getByText('General Surgery (Version 1)')).toBeInTheDocument();
      expect(screen.getByText('Cardiac Surgery (Version 2)')).toBeInTheDocument();
    });
  });

  test('successfully creates a new operation request', async () => {
    operationRequestService.createOperationRequest.mockResolvedValue({ ...mockOperationData, id: 1 });
    render(<CreateOperationRequest />);

    // Wait for operation types to load
    await waitFor(() => {
      expect(screen.getByText('General Surgery (Version 1)')).toBeInTheDocument();
    });

    await act(async () => {
      fireEvent.change(screen.getByLabelText('Patient Medical Record Number:'), 
        { target: { value: mockOperationData.patientMRN } });
      fireEvent.change(screen.getByLabelText('Doctor License Number:'), 
        { target: { value: mockOperationData.doctorLicenseNumber } });
      
      // Select operation type using the select element
      const operationTypeSelect = screen.getByLabelText('Operation Type:');
      fireEvent.change(operationTypeSelect, { target: { value: mockOperationData.operationTypeId } });
      
      fireEvent.change(screen.getByLabelText('Deadline:'), 
        { target: { value: mockOperationData.deadline } });
      fireEvent.change(screen.getByLabelText('Priority:'), 
        { target: { value: mockOperationData.priority } });
    });

    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /submit request/i }));
    });

    await waitFor(() => {
      expect(operationRequestService.createOperationRequest).toHaveBeenCalledWith(mockOperationData);
      expect(screen.getByText('Operation request created successfully!')).toBeInTheDocument();
    });
  });

  test('displays error message when operation types fail to load', async () => {
    operationRequestService.getAllOperationTypes.mockRejectedValue(new Error('Failed to fetch'));
    render(<CreateOperationRequest />);
    
    await waitFor(() => {
      expect(screen.getByText('Error fetching operation types')).toBeInTheDocument();
    });
  });

  test('displays error message on failed creation', async () => {
    operationRequestService.createOperationRequest.mockRejectedValue(new Error('API Error'));
    render(<CreateOperationRequest />);

    await waitFor(() => {
      expect(screen.getByLabelText('Operation Type:')).toBeInTheDocument();
    });

    await act(async () => {
      fireEvent.change(screen.getByLabelText('Patient Medical Record Number:'), 
        { target: { value: 'P123' } });
      fireEvent.change(screen.getByLabelText('Doctor License Number:'), 
        { target: { value: 'LIC-12345678' } });
      const operationTypeSelect = screen.getByLabelText('Operation Type:');
      fireEvent.change(operationTypeSelect, { target: { value: 'OT789' } });
      fireEvent.change(screen.getByLabelText('Deadline:'), 
        { target: { value: '2024-12-01T10:00' } });
      fireEvent.change(screen.getByLabelText('Priority:'), 
        { target: { value: 'urgent' } });
    });

    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /submit request/i }));
    });

    await waitFor(() => {
      expect(screen.getByText('Error creating operation request.')).toBeInTheDocument();
    });
  });
});