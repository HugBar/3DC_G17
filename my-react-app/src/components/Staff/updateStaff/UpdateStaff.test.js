import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { MemoryRouter, Routes, Route } from 'react-router-dom';
import UpdateStaff from './UpdateStaff';
import staffService from '../../../api/staffService';

// Mock the staffService
jest.mock('../../../api/staffService');

const mockStaffData = {
  id: 1,
  firstName: 'John',
  lastName: 'Doe',
  email: 'stafftest@stafftest.com',
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
    // Mock localStorage
    Storage.prototype.getItem = jest.fn(() => 'fake-token');
  });

  const renderUpdateStaff = () => {
    return render(
      <MemoryRouter initialEntries={['/staff/update/1']}>
        <Routes>
          <Route path="/staff/update/:id" element={<UpdateStaff onBack={jest.fn()} />} />
        </Routes>
      </MemoryRouter>
    );
  };

  test('renders update form with staff data', async () => {
    renderUpdateStaff();

    // Wait for the data to be loaded
    await screen.findByDisplayValue(mockStaffData.email);
    
    expect(screen.getByDisplayValue(mockStaffData.email)).toBeInTheDocument();
    expect(screen.getByDisplayValue(mockStaffData.phoneNumber)).toBeInTheDocument();
    expect(screen.getByDisplayValue(mockStaffData.specialization)).toBeInTheDocument();
  });

  test('updates fields and submits updated staff data', async () => {
    staffService.updateStaff.mockResolvedValue({});
    renderUpdateStaff();

    // Wait for form to be populated
    await screen.findByDisplayValue(mockStaffData.email);

    // Update fields
    fireEvent.change(screen.getByLabelText(/phone number/i), { target: { value: '987654321' } });
    fireEvent.change(screen.getByLabelText(/specialization/i), { target: { value: 'Neurology' } });

    // Submit form
    fireEvent.click(screen.getByRole('button', { name: /update/i }));

    // Wait for the update to complete
    await screen.findByText('Staff updated successfully!');

    expect(staffService.updateStaff).toHaveBeenCalledWith('1', {
      ...mockStaffData,
      phoneNumber: '987654321',
      specialization: 'Neurology',
    });
  });

  test('displays error message on failed update', async () => {
    staffService.updateStaff.mockRejectedValue(new Error('API Error'));
    renderUpdateStaff();

    // Wait for form to be populated
    await screen.findByDisplayValue(mockStaffData.email);

    // Submit form
    fireEvent.click(screen.getByRole('button', { name: /update/i }));

    // Check for error message
    expect(await screen.findByText('Error updating staff.')).toBeInTheDocument();
  });
});