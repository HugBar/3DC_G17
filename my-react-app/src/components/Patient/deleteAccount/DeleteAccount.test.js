import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { BrowserRouter, useNavigate } from 'react-router-dom';
import { useAuth } from '../../../context/AuthContext';
import DeleteAccount from './DeleteAccount';
import patientService from '../../../api/patientService';

jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: jest.fn()
}));

jest.mock('../../../context/AuthContext', () => ({
  useAuth: jest.fn()
}));

jest.mock('../../../api/patientService');

describe('DeleteAccount Component', () => {
  const mockNavigate = jest.fn();
  const mockLogout = jest.fn();
  const mockUserEmail = 'test@example.com';

  beforeEach(() => {
    jest.clearAllMocks();
    jest.spyOn(console, 'error').mockImplementation(() => {});
    useNavigate.mockImplementation(() => mockNavigate);
    useAuth.mockImplementation(() => ({
      userEmail: mockUserEmail,
      logout: mockLogout
    }));
  });

  afterEach(() => {
    console.error.mockRestore();
  });

  const renderDeleteAccount = () => {
    return render(
      <BrowserRouter>
        <DeleteAccount />
      </BrowserRouter>
    );
  };

  test('renders delete account component', () => {
    renderDeleteAccount();
    
    expect(screen.getByText('Delete Account')).toBeInTheDocument();
    expect(screen.getByText(/This action is irreversible/)).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /Delete My Account/i })).toBeInTheDocument();
    
    // Check for data retention information
    expect(screen.getByText(/Important Information About Data Retention/)).toBeInTheDocument();
    expect(screen.getByText(/Medical History Records/)).toBeInTheDocument();
    expect(screen.getByText(/Appointment History/)).toBeInTheDocument();
    expect(screen.getByText(/Age Range/)).toBeInTheDocument();
    expect(screen.getByText(/Gender/)).toBeInTheDocument();
  });

  test('solicita token de confirmação com sucesso', async () => {
    patientService.requestAccountDeletion.mockResolvedValue({});
    renderDeleteAccount();

    await act(async () => {
      fireEvent.click(screen.getByText('Delete My Account'));
    });

    expect(patientService.requestAccountDeletion).toHaveBeenCalledWith(mockUserEmail);
    expect(screen.getByText('Enter Verification Code')).toBeInTheDocument();
  });

  test('confirma exclusão da conta com sucesso', async () => {
    patientService.requestAccountDeletion.mockResolvedValue({});
    patientService.confirmAccountDeletion.mockResolvedValue({});
    renderDeleteAccount();

    await act(async () => {
      fireEvent.click(screen.getByText('Delete My Account'));
    });

    const tokenInput = screen.getByPlaceholderText('000000');
    fireEvent.change(tokenInput, { target: { value: '123456' } });

    await act(async () => {
      fireEvent.click(screen.getByText('Confirm Deletion'));
    });

    expect(patientService.confirmAccountDeletion).toHaveBeenCalledWith('123456');
    expect(screen.getByText('Account successfully deleted.')).toBeInTheDocument();
  });

  test('redireciona para login após exclusão bem-sucedida', async () => {
    jest.useFakeTimers();
    patientService.requestAccountDeletion.mockResolvedValue({});
    patientService.confirmAccountDeletion.mockResolvedValue({});
    renderDeleteAccount();

    await act(async () => {
      fireEvent.click(screen.getByText('Delete My Account'));
    });

    const tokenInput = screen.getByPlaceholderText('000000');
    fireEvent.change(tokenInput, { target: { value: '123456' } });

    await act(async () => {
      fireEvent.click(screen.getByText('Confirm Deletion'));
    });

    await act(async () => {
      jest.advanceTimersByTime(3000);
    });

    expect(mockLogout).toHaveBeenCalled();
    expect(mockNavigate).toHaveBeenCalledWith('/login');
    jest.useRealTimers();
  });

 
test('exibe erro quando token é inválido', async () => {
  patientService.requestAccountDeletion.mockResolvedValue({});
  patientService.confirmAccountDeletion.mockRejectedValue(new Error('Invalid verification code'));
  renderDeleteAccount();

  await act(async () => {
    fireEvent.click(screen.getByText('Delete My Account'));
  });

  const tokenInput = screen.getByPlaceholderText('000000');
  fireEvent.change(tokenInput, { target: { value: '111111' } }); // Using invalid digits instead of text

  await act(async () => {
    fireEvent.click(screen.getByText('Confirm Deletion'));
  });

  await act(async () => {
    const errorMessage = await screen.findByText('Invalid verification code or deletion failed. Please try again.');
    expect(errorMessage).toBeInTheDocument();
  });
});

  test('exibe erro quando tenta confirmar sem token', async () => {
    patientService.requestAccountDeletion.mockResolvedValue({});
    renderDeleteAccount();

    await act(async () => {
      fireEvent.click(screen.getByText('Delete My Account'));
    });

    await act(async () => {
      fireEvent.click(screen.getByText('Confirm Deletion'));
    });

    expect(screen.getByText('Please enter the 6-digit verification code from your email.')).toBeInTheDocument();
  });

  test('fecha modal ao clicar em Cancel', async () => {
    patientService.requestAccountDeletion.mockResolvedValue({});
    renderDeleteAccount();

    await act(async () => {
      fireEvent.click(screen.getByText('Delete My Account'));
    });

    fireEvent.click(screen.getByText('Cancel'));
    expect(screen.queryByText('Enter Verification Code')).not.toBeInTheDocument();
  });
});