import React from 'react';
import { render, screen, fireEvent, waitFor, act } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import PatientListDoctor from './PatientListDoctor';
import patientService from '../../../api/patientService';

// Mock the patient service
jest.mock('../../../api/patientService');

// Mock useNavigate
const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate
}));

const mockPatients = {
  items: [
    {
      id: 1,
      firstName: 'John',
      lastName: 'Doe',
      medicalNr: 'MED123',
      email: 'john@example.com',
      phoneNumber: '123456789'
    },
    {
      id: 2,
      firstName: 'Jane',
      lastName: 'Smith',
      medicalNr: 'MED456',
      email: 'jane@example.com',
      phoneNumber: '987654321'
    }
  ],
  totalPages: 2,
  currentPage: 1,
  pageSize: 1,
  totalCount: 4
};

describe('PatientListDoctor', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    patientService.getAllPatients.mockResolvedValue(mockPatients);
  });

  const renderComponent = () => {
    return render(
      <BrowserRouter>
        <PatientListDoctor />
      </BrowserRouter>
    );
  };

  test('renders patient list with correct data', async () => {
    renderComponent();

    await waitFor(() => {
      expect(screen.getByText('John Doe')).toBeInTheDocument();
      expect(screen.getByText('Jane Smith')).toBeInTheDocument();
    });
  });

  test('handles filter changes', async () => {
    await act(async () => {
      renderComponent();
    });
  
    await act(async () => {
      const firstNameInput = screen.getByPlaceholderText('First Name');
      fireEvent.change(firstNameInput, { target: { value: 'John' } });
    });
  
    await waitFor(() => {
      expect(patientService.getAllPatients).toHaveBeenLastCalledWith(
        expect.objectContaining({ firstName: 'John' }),
        1,
        1
      );
    });
  });

  test('clears filters when clear button is clicked', async () => {
    renderComponent();

    const firstNameInput = screen.getByPlaceholderText('First Name');
    fireEvent.change(firstNameInput, { target: { value: 'John' } });
    
    const clearButton = screen.getByText('Clear Filters');
    fireEvent.click(clearButton);

    await waitFor(() => {
      expect(firstNameInput.value).toBe('');
    });
  });

  test('navigates to patient profile when card is clicked', async () => {
    await act(async () => {
      renderComponent();
    });

    await act(async () => {
      const patientCard = screen.getByText('John Doe').closest('.patient-card');
      fireEvent.click(patientCard);
    });

    expect(mockNavigate).toHaveBeenCalledWith('/patient/1');
  });

  test('handles pagination correctly', async () => {
    await act(async () => {
      renderComponent();
    });

    await act(async () => {
      const nextButton = screen.getByText('Next');
      fireEvent.click(nextButton);
    });

    expect(mockNavigate).toHaveBeenCalledWith(expect.stringContaining('page=2'));
  });

  test('displays error message when API call fails', async () => {
    patientService.getAllPatients.mockRejectedValueOnce(new Error('API Error'));
    
    await act(async () => {
      renderComponent();
    });

    await waitFor(() => {
      expect(screen.getByText('Error fetching patient list.')).toBeInTheDocument();
    });
  });

  test('disables previous button on first page', async () => {
    await act(async () => {
      renderComponent();
    });

    await waitFor(() => {
      const prevButton = screen.getByText('Previous');
      expect(prevButton).toBeDisabled();
    });
  });

  test('disables next button on last page', async () => {
    patientService.getAllPatients.mockResolvedValueOnce({
      ...mockPatients,
      currentPage: 2,
      totalPages: 2
    });
    
    renderComponent();

    await waitFor(() => {
      const nextButton = screen.getByText('Next');
      expect(nextButton).toBeDisabled();
    });
  });
});