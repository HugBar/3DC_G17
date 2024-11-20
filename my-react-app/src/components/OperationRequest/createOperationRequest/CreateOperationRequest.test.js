import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import CreateOperationRequest from './CreateOperationRequest';
import operationRequestService from '../../../api/operationRequestService';

jest.mock('../../../api/operationRequestService', () => ({
  __esModule: true,
  default: {
    createOperationRequest: jest.fn(),
  },
}));

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
  });

  test('renders create operation request form', () => {
    render(<CreateOperationRequest />);
    
    expect(screen.getByText('Create Operation Request')).toBeInTheDocument();
    expect(screen.getByLabelText('Patient Medical Record Number:')).toBeInTheDocument();
    expect(screen.getByLabelText('Doctor License Number:')).toBeInTheDocument();
    expect(screen.getByLabelText('Operation Type ID:')).toBeInTheDocument();
    expect(screen.getByLabelText('Deadline:')).toBeInTheDocument();
    expect(screen.getByLabelText('Priority:')).toBeInTheDocument();
  });

  test('successfully creates a new operation request', async () => {
    operationRequestService.createOperationRequest.mockResolvedValue({ ...mockOperationData, id: 1 });
    render(<CreateOperationRequest />);

    await act(async () => {
      fireEvent.change(screen.getByLabelText('Patient Medical Record Number:'), 
        { target: { value: mockOperationData.patientMRN } });
      fireEvent.change(screen.getByLabelText('Doctor License Number:'), 
        { target: { value: mockOperationData.doctorLicenseNumber } });
      fireEvent.change(screen.getByLabelText('Operation Type ID:'), 
        { target: { value: mockOperationData.operationTypeId } });
      fireEvent.change(screen.getByLabelText('Deadline:'), 
        { target: { value: mockOperationData.deadline } });
      fireEvent.change(screen.getByLabelText('Priority:'), 
        { target: { value: mockOperationData.priority } });
    });

    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /submit request/i }));
    });

    expect(operationRequestService.createOperationRequest).toHaveBeenCalledWith(mockOperationData);
  });

  test('displays error message on failed creation', async () => {
    operationRequestService.createOperationRequest.mockRejectedValue(new Error('API Error'));
    render(<CreateOperationRequest />);

    // Fill in minimal required data
    await act(async () => {
      fireEvent.change(screen.getByLabelText('Patient Medical Record Number:'), 
        { target: { value: 'P123' } });
      fireEvent.change(screen.getByLabelText('Doctor License Number:'), 
        { target: { value: 'LIC-12345678' } });
    });

    // Submit form
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /submit request/i }));
    });

    expect(screen.getByText('Error creating operation request.')).toBeInTheDocument();
  });

  test('form clears after successful submission', async () => {
    operationRequestService.createOperationRequest.mockResolvedValue({ ...mockOperationData, id: 1 });
    render(<CreateOperationRequest />);

    // Fill in the form
    await act(async () => {
      fireEvent.change(screen.getByLabelText('Patient Medical Record Number:'), 
        { target: { value: mockOperationData.patientMRN } });
      fireEvent.change(screen.getByLabelText('Doctor License Number:'), 
        { target: { value: mockOperationData.doctorLicenseNumber } });
      fireEvent.change(screen.getByLabelText('Operation Type ID:'), 
        { target: { value: mockOperationData.operationTypeId } });
      fireEvent.change(screen.getByLabelText('Deadline:'), 
        { target: { value: mockOperationData.deadline } });
      fireEvent.change(screen.getByLabelText('Priority:'), 
        { target: { value: mockOperationData.priority } });
    });

    // Submit form
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /submit request/i }));
    });

    // Verify form is cleared
    expect(screen.getByLabelText('Patient Medical Record Number:')).toHaveValue('');
    expect(screen.getByLabelText('Doctor License Number:')).toHaveValue('');
    expect(screen.getByLabelText('Operation Type ID:')).toHaveValue('');
    expect(screen.getByLabelText('Deadline:')).toHaveValue('');
    expect(screen.getByLabelText('Priority:')).toHaveValue('elective');
  });
});