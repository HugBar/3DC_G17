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

  test('renderiza o componente de exclusão de conta', () => {
    renderDeleteAccount();
    
    expect(screen.getByText('Delete Account')).toBeInTheDocument();
    expect(screen.getByText(/Warning: This action cannot be undone/)).toBeInTheDocument();
    expect(screen.getByText('Delete My Account')).toBeInTheDocument();
  });

  test('solicita token de confirmação com sucesso', async () => {
    patientService.requestAccountDeletion.mockResolvedValue({});
    renderDeleteAccount();

    await act(async () => {
      fireEvent.click(screen.getByText('Delete My Account'));
    });

    expect(patientService.requestAccountDeletion).toHaveBeenCalledWith(mockUserEmail);
    expect(screen.getByText('Enter Confirmation Token')).toBeInTheDocument();
  });

  test('confirma exclusão da conta com sucesso', async () => {
    patientService.requestAccountDeletion.mockResolvedValue({});
    patientService.confirmAccountDeletion.mockResolvedValue({});
    renderDeleteAccount();

    await act(async () => {
      fireEvent.click(screen.getByText('Delete My Account'));
    });

    const tokenInput = screen.getByPlaceholderText('Enter token');
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

    const tokenInput = screen.getByPlaceholderText('Enter token');
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
    patientService.confirmAccountDeletion.mockRejectedValue(new Error('Invalid token'));
    renderDeleteAccount();

    await act(async () => {
      fireEvent.click(screen.getByText('Delete My Account'));
    });

    const tokenInput = screen.getByPlaceholderText('Enter token');
    fireEvent.change(tokenInput, { target: { value: 'invalid-token' } });

    await act(async () => {
      fireEvent.click(screen.getByText('Confirm Deletion'));
    });

    expect(screen.getByText('Invalid token or deletion failed. Please try again.')).toBeInTheDocument();
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

    expect(screen.getByText('Please enter the confirmation token from your email.')).toBeInTheDocument();
  });

  test('fecha modal ao clicar em Cancel', async () => {
    patientService.requestAccountDeletion.mockResolvedValue({});
    renderDeleteAccount();

    await act(async () => {
      fireEvent.click(screen.getByText('Delete My Account'));
    });

    fireEvent.click(screen.getByText('Cancel'));
    expect(screen.queryByText('Enter Confirmation Token')).not.toBeInTheDocument();
  });
});