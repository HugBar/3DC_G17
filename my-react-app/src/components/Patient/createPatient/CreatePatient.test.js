import React, { useEffect, useState } from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { act } from 'react';
import CreatePatient from './CreatePatient';
import { useNavigate } from 'react-router-dom';
import usePatientFormValidation from '../../../hooks/usePatientFormValidation';
import patientService from '../../../api/patientService';
import { jwtDecode } from 'jwt-decode';

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

jest.mock('jwt-decode', () => ({
  jwtDecode: jest.fn()
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
    patientService.registerPatientItself.mockResolvedValue({});
    patientService.registerPatient.mockResolvedValue({});
    // Mock JWT token and its decoded value
    Storage.prototype.getItem = jest.fn(() => 'fake-token');
    jwtDecode.mockReturnValue({
      email: 'patient@test.com'
    });
    // Mock console.error
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  afterEach(() => {
    console.error.mockRestore();
  });

  test('renders create patient form for admin', () => {
    render(<CreatePatient isAdmin={true} />);
    
    expect(screen.getByText('Register New Patient')).toBeInTheDocument();
    expect(screen.getByLabelText('First Name:')).toBeInTheDocument();
    expect(screen.getByLabelText('Last Name:')).toBeInTheDocument();
    expect(screen.getByLabelText('Email:')).toBeInTheDocument();
    expect(screen.getByLabelText('Phone Number:')).toBeInTheDocument();
  });

  test('renders create patient form for patient without email field', () => {
    render(<CreatePatient isAdmin={false} />);
    
    expect(screen.getByText('Register New Patient')).toBeInTheDocument();
    expect(screen.getByLabelText('First Name:')).toBeInTheDocument();
    expect(screen.getByLabelText('Last Name:')).toBeInTheDocument();
    expect(screen.queryByLabelText('Email:')).not.toBeInTheDocument();
    expect(screen.getByLabelText('Phone Number:')).toBeInTheDocument();
  });

  test('successfully registers a new patient as admin', async () => {
    mockValidate.mockReturnValue(true);
    render(<CreatePatient isAdmin={true} />);

    await act(async () => {
      fireEvent.change(screen.getByLabelText('First Name:'), { target: { value: mockPatientData.firstName } });
      fireEvent.change(screen.getByLabelText('Last Name:'), { target: { value: mockPatientData.lastName } });
      fireEvent.change(screen.getByLabelText('Email:'), { target: { value: mockPatientData.email } });
      fireEvent.change(screen.getByLabelText('Phone Number:'), { target: { value: mockPatientData.phoneNumber } });
      fireEvent.click(screen.getByRole('button', { name: /register patient/i }));
    });

    expect(patientService.registerPatient).toHaveBeenCalledWith(
      expect.objectContaining({
        email: mockPatientData.email
      })
    );
  });

  test('successfully registers a new patient as patient', async () => {
    mockValidate.mockReturnValue(true);
    render(<CreatePatient isAdmin={false} />);

    await act(async () => {
      fireEvent.change(screen.getByLabelText('First Name:'), { target: { value: mockPatientData.firstName } });
      fireEvent.change(screen.getByLabelText('Last Name:'), { target: { value: mockPatientData.lastName } });
      fireEvent.change(screen.getByLabelText('Phone Number:'), { target: { value: mockPatientData.phoneNumber } });
      fireEvent.click(screen.getByRole('button', { name: /register patient/i }));
    });

    expect(patientService.registerPatientItself).toHaveBeenCalledWith(
      expect.objectContaining({
        email: 'patient@test.com'
      })
    );
  });

  test('handles invalid JWT token', async () => {
    jwtDecode.mockImplementation(() => {
      throw new Error('Invalid token');
    });
    
    render(<CreatePatient isAdmin={false} />);
    
    expect(console.error).toHaveBeenCalledWith(
      'Error decoding token:',
      expect.any(Error)
    );
  });
});
