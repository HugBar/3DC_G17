import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import UpdateStaff from './UpdateStaff';
import staffService from '../../../api/staffService';

jest.mock('../../../api/staffService', () => ({
  __esModule: true,
  default: {
    getStaffById: jest.fn(),
    updateStaff: jest.fn(),
  },
}));

const mockStaffData = {
  id: 1,
  firstName: 'John',
  lastName: 'Doe',
  email: 'john@example.com',
  phoneNumber: '123456789',
  specialization: 'Cardiology',
  availabilitySlots: [
    { startTime: '2023-11-07T09:00', endTime: '2023-11-07T11:00' },
  ],
};

describe('UpdateStaff Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    staffService.getStaffById.mockResolvedValue(mockStaffData);
  });

  test('renders update form with staff data', async () => {
    await act(async () => {
      render(<UpdateStaff staffId={1} />);
    });

    expect(await screen.findByDisplayValue('john@example.com')).toBeInTheDocument();
    expect(screen.getByDisplayValue('123456789')).toBeInTheDocument();
    expect(screen.getByDisplayValue('Cardiology')).toBeInTheDocument();
  });

  test('updates fields and submits updated staff data', async () => {
    staffService.updateStaff.mockResolvedValue({});

    await act(async () => {
      render(<UpdateStaff staffId={1} />);
    });

    // Update fields
    fireEvent.change(screen.getByLabelText(/phone number/i), { target: { value: '987654321' } });
    fireEvent.change(screen.getByLabelText(/specialization/i), { target: { value: 'Neurology' } });

    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /update/i }));
    });

    expect(staffService.updateStaff).toHaveBeenCalledWith(1, {
      ...mockStaffData,
      phoneNumber: '987654321',
      specialization: 'Neurology',
    });
  });

  test('displays error message on failed update', async () => {
    staffService.updateStaff.mockRejectedValue(new Error('API Error'));

    await act(async () => {
      render(<UpdateStaff staffId={1} />);
    });

    fireEvent.change(screen.getByLabelText(/phone number/i), { target: { value: '987654321' } });
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /update/i }));
    });

    expect(await screen.findByText('Error updating staff.')).toBeInTheDocument();
  });
});
