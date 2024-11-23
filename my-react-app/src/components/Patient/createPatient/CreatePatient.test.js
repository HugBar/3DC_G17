import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { act } from 'react';
import CreatePatient from './CreatePatient';
import { useNavigate } from 'react-router-dom';
import usePatientFormValidation from '../../../hooks/usePatientFormValidation';
import patientService from '../../../api/patientService';

// Mock the dependencies
jest.mock('react-router-dom', () => ({
  useNavigate: jest.fn(),
}));

jest.mock('../../../hooks/usePatientFormValidation', () => ({
  __esModule: true,
  default: jest.fn(),
}));

jest.mock('../../../api/patientService', () => ({
  __esModule: true,
  default: {
    registerPatientItself: jest.fn(),
    registerPatient: jest.fn(),
  },
}));

const mockPatientData = {
  firstName: 'John',
  lastName: 'Doe',
  email: 'john@example.com',
  phoneNumber: '123456789',
  dateOfBirth: '1990-01-01',
  gender: 'Male',
  contactInfo: 'Some contact info',
  emergencyContact: 'Emergency contact info',
  medicalNr: '12345'
};

describe('CreatePatient Component', () => {
  const mockNavigate = jest.fn();
  const mockValidate = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
    useNavigate.mockReturnValue(mockNavigate);
    usePatientFormValidation.mockReturnValue({
      errors: {},
      validate: mockValidate,
    });
    // Mock successful patient registration
    patientService.registerPatientItself.mockResolvedValue({});
    patientService.registerPatient.mockResolvedValue({});
  });

  test('renders create patient form', () => {
    render(<CreatePatient />);
    
    expect(screen.getByText('Register New Patient')).toBeInTheDocument();
    expect(screen.getByLabelText('First Name:')).toBeInTheDocument();
    expect(screen.getByLabelText('Last Name:')).toBeInTheDocument();
    expect(screen.getByLabelText('Email:')).toBeInTheDocument();
    expect(screen.getByLabelText('Phone Number:')).toBeInTheDocument();
    expect(screen.getByLabelText('Date of Birth:')).toBeInTheDocument();
    expect(screen.getByLabelText('Gender:')).toBeInTheDocument();
  });

  test('successfully registers a new patient', async () => {
    mockValidate.mockReturnValue(true);
    render(<CreatePatient />);

    // Fill in the form
    await act(async () => {
      fireEvent.change(screen.getByLabelText('First Name:'), { target: { value: mockPatientData.firstName } });
      fireEvent.change(screen.getByLabelText('Last Name:'), { target: { value: mockPatientData.lastName } });
      fireEvent.change(screen.getByLabelText('Email:'), { target: { value: mockPatientData.email } });
      fireEvent.change(screen.getByLabelText('Phone Number:'), { target: { value: mockPatientData.phoneNumber } });
      fireEvent.change(screen.getByLabelText('Date of Birth:'), { target: { value: mockPatientData.dateOfBirth } });
      fireEvent.change(screen.getByLabelText('Gender:'), { target: { value: mockPatientData.gender } });
      fireEvent.change(screen.getByLabelText('Contact Info:'), { target: { value: mockPatientData.contactInfo } });
      fireEvent.change(screen.getByLabelText('Emergency Contact:'), { target: { value: mockPatientData.emergencyContact } });
    });

    // Submit form
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /register patient/i }));
    });

    await waitFor(() => {
      expect(screen.getByText('Patient registered successfully!')).toBeInTheDocument();
    });

    // Check if navigation is called after successful registration
    await waitFor(() => {
      expect(mockNavigate).toHaveBeenCalledWith('/patient/update');
    }, { timeout: 3000 });
  });

  test('displays validation errors when form is submitted with invalid data', async () => {
    const mockErrors = {
      firstName: 'First name is required',
      email: 'Invalid email format'
    };

    usePatientFormValidation.mockReturnValue({
      errors: mockErrors,
      validate: jest.fn().mockReturnValue(false)
    });

    render(<CreatePatient />);

    // Submit form without filling required fields
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /register patient/i }));
    });

    expect(screen.getByText('First name is required')).toBeInTheDocument();
    expect(screen.getByText('Invalid email format')).toBeInTheDocument();
  });

  test('handles cancel button click', async () => {
    render(<CreatePatient />);

    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: /cancel/i }));
    });

    expect(mockNavigate).toHaveBeenCalledWith('/patient/list');
  });
});
