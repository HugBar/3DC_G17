import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { BrowserRouter, useNavigate } from 'react-router-dom';
import UpdatePatient from './UpdatePatient';
import patientService from '../../../api/patientService';

jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: jest.fn()
}));

jest.mock('../../../api/patientService');

const mockPatientData = {
  email: 'patient@test.com',
  phoneNumber: '123456789',
  firstName: 'John',
  lastName: 'Doe',
  dateOfBirth: '1990-01-01',
  gender: 'Male',
  contactInfo: 'Contact info test',
  emergencyContact: 'Emergency contact test',
  medicalHistory: 'Medical history test'
};

describe('UpdatePatient Component', () => {
  const mockNavigate = jest.fn();
  
  beforeEach(() => {
    jest.clearAllMocks();
    jest.spyOn(console, 'error').mockImplementation(() => {});
    useNavigate.mockImplementation(() => mockNavigate);
    patientService.getPatientProfile.mockResolvedValue(mockPatientData);
    Storage.prototype.removeItem = jest.fn();
    Storage.prototype.getItem = jest.fn(() => 'patient@test.com');
  });

  afterEach(() => {
    console.error.mockRestore();
  });

  const renderUpdatePatient = () => {
    return render(
      <BrowserRouter>
        <UpdatePatient patientEmail="patient@test.com" onBack={jest.fn()} />
      </BrowserRouter>
    );
  };

  test('renderiza formulário de atualização com dados do paciente', async () => {
    await act(async () => {
      renderUpdatePatient();
    });

    const emailInput = screen.getByDisplayValue(mockPatientData.email);
    const phoneInput = screen.getByDisplayValue(mockPatientData.phoneNumber);
    const firstNameInput = screen.getByDisplayValue(mockPatientData.firstName);
    const lastNameInput = screen.getByDisplayValue(mockPatientData.lastName);

    expect(emailInput).toBeInTheDocument();
    expect(phoneInput).toBeInTheDocument();
    expect(firstNameInput).toBeInTheDocument();
    expect(lastNameInput).toBeInTheDocument();
  });

  test('exibe erro quando email é inválido', async () => {
    await act(async () => {
      renderUpdatePatient();
    });

    const emailInput = screen.getByDisplayValue(mockPatientData.email);
    fireEvent.change(emailInput, { target: { value: 'invalid-email' } });
    
    const submitButton = screen.getByText('Update');
    fireEvent.click(submitButton);

    const errorElement = screen.getByText('Please enter a valid email.');
    expect(errorElement).toBeInTheDocument();
  });

  test('exibe erro quando número de telefone é inválido', async () => {
    await act(async () => {
      renderUpdatePatient();
    });

    const phoneInput = screen.getByDisplayValue(mockPatientData.phoneNumber);
    fireEvent.change(phoneInput, { target: { value: '12345' } });
    
    const submitButton = screen.getByText('Update');
    fireEvent.click(submitButton);

    const errorElement = screen.getByText('Phone number must be exactly 9 digits.');
    expect(errorElement).toBeInTheDocument();
  });


  test('exibe modal quando email é alterado', async () => {
    patientService.updatePatientProfile.mockResolvedValue({});
    
    await act(async () => {
      renderUpdatePatient();
    });

    const emailInput = screen.getByDisplayValue(mockPatientData.email);
    fireEvent.change(emailInput, { target: { value: 'newemail@test.com' } });

    const submitButton = screen.getByText('Update');
    await act(async () => {
      fireEvent.click(submitButton);
    });

    const modalElement = screen.getByText('Email Change');
    expect(modalElement).toBeInTheDocument();
  });

  test('redireciona para login após confirmação de mudança de email', async () => {
    patientService.updatePatientProfile.mockResolvedValue({});
    
    await act(async () => {
      renderUpdatePatient();
    });

    const emailInput = screen.getByDisplayValue(mockPatientData.email);
    fireEvent.change(emailInput, { target: { value: 'newemail@test.com' } });

    const submitButton = screen.getByText('Update');
    await act(async () => {
      fireEvent.click(submitButton);
    });

    const okButton = screen.getByText('OK');
    fireEvent.click(okButton);

    expect(localStorage.removeItem).toHaveBeenCalledWith('authToken');
    expect(mockNavigate).toHaveBeenCalledWith('/login');
  });

  test('exibe mensagem de erro quando atualização falha', async () => {
    patientService.updatePatientProfile.mockRejectedValue({ 
      response: { data: 'Failed to update profile' }
    });
    
    await act(async () => {
      renderUpdatePatient();
    });

    const phoneInput = screen.getByDisplayValue(mockPatientData.phoneNumber);
    fireEvent.change(phoneInput, { target: { value: '987654321' } });

    const submitButton = screen.getByText('Update');
    await act(async () => {
      fireEvent.click(submitButton);
    });

    expect(console.error).toHaveBeenCalledWith(
      'Failed to update profile:',
      expect.any(Object)
    );
  });

  test('exibe mensagem quando nenhuma alteração é detectada', async () => {
    await act(async () => {
      renderUpdatePatient();
    });

    const submitButton = screen.getByText('Update');
    await act(async () => {
      fireEvent.click(submitButton);
    });

    expect(patientService.updatePatientProfile).not.toHaveBeenCalled();
  });

  test('handles missing patient email', async () => {
    Storage.prototype.getItem = jest.fn(() => null);
    
    await act(async () => {
      renderUpdatePatient();
    });

    expect(patientService.getPatientProfile).not.toHaveBeenCalled();
  });

  test('retrieves email from localStorage when not in location state', async () => {
    const localStorageEmail = 'patient@localStorage.com';
    Storage.prototype.getItem = jest.fn(() => localStorageEmail);
    
    await act(async () => {
      renderUpdatePatient();
    });

    expect(patientService.getPatientProfile).toHaveBeenCalledWith(localStorageEmail);
  });

  test('displays success message when update is successful', async () => {
    patientService.updatePatientProfile.mockResolvedValue({});
    
    await act(async () => {
      renderUpdatePatient();
    });

    const phoneInput = screen.getByDisplayValue(mockPatientData.phoneNumber);
    fireEvent.change(phoneInput, { target: { value: '987654321' } });

    const submitButton = screen.getByText('Update');
    await act(async () => {
      fireEvent.click(submitButton);
    });

    const successMessage = screen.getByText('Profile updated successfully');
    expect(successMessage).toBeInTheDocument();
    expect(successMessage.className).toContain('success-message');
  });

  test('displays error message when profile fetch fails', async () => {
    patientService.getPatientProfile.mockRejectedValue(new Error('Failed to fetch'));
    
    await act(async () => {
      renderUpdatePatient();
    });

    const errorMessage = screen.getByText('Failed to fetch patient data');
    expect(errorMessage).toBeInTheDocument();
    expect(errorMessage.className).toContain('error-message');
  });
});