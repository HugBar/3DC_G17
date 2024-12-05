import React from 'react';
import { render, screen, fireEvent, act, waitFor } from '@testing-library/react';
import { MemoryRouter, Routes, Route } from 'react-router-dom';
import UpdateStaff from './UpdateStaff';
import staffService from '../../../api/staffService';
import specializationService from '../../../api/specializationService';

// Mock the services
jest.mock('../../../api/staffService');
jest.mock('../../../api/specializationService');

const mockStaffData = {
  id: 1,
  email: 'stafftest@stafftest.com',
  phoneNumber: '123456789',
  specialization: 'Cardiology',
  availabilitySlots: [
    { startTime: '2023-11-07T09:00', endTime: '2023-11-07T11:00' },
  ],
};

const mockSpecializations = [
  { _id: '1', name: 'Cardiology' },
  { _id: '2', name: 'Neurology' }
];

describe('UpdateStaff Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    jest.spyOn(console, 'error').mockImplementation(() => {});
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'warn').mockImplementation(() => {});
    
    staffService.getStaffById.mockResolvedValue(mockStaffData);
    specializationService.getAllSpecializations.mockResolvedValue(mockSpecializations);
    Storage.prototype.getItem = jest.fn(() => 'fake-token');
  });

  afterEach(() => {
    console.error.mockRestore();
    console.log.mockRestore();
    console.warn.mockRestore();
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

  test('renders update form with staff data and specializations', async () => {
    renderUpdateStaff();

    await waitFor(() => {
      expect(screen.getByDisplayValue(mockStaffData.email)).toBeInTheDocument();
      expect(screen.getByDisplayValue(mockStaffData.phoneNumber)).toBeInTheDocument();
      expect(screen.getByRole('combobox')).toHaveValue(mockStaffData.specialization);
    });
  });

  

  test('displays error message when specializations fail to load', async () => {
    specializationService.getAllSpecializations.mockRejectedValue(new Error('Failed to load'));
    renderUpdateStaff();

    await waitFor(() => {
      expect(screen.getByText('Error loading specializations')).toBeInTheDocument();
    });
  });
});