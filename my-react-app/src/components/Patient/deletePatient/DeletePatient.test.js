import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { BrowserRouter, useNavigate, useParams } from 'react-router-dom';
import DeletePatient from './DeletePatient';
import patientService from '../../../api/patientService';

jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: jest.fn(),
  useParams: jest.fn()
}));

jest.mock('../../../api/patientService');

describe('DeletePatient Component', () => {
  const mockNavigate = jest.fn();
  const mockPatientId = '123';

  beforeEach(() => {
    jest.clearAllMocks();
    jest.spyOn(console, 'error').mockImplementation(() => {});
    useNavigate.mockImplementation(() => mockNavigate);
    useParams.mockImplementation(() => ({ id: mockPatientId }));
  });

  afterEach(() => {
    console.error.mockRestore();
  });

  const renderDeletePatient = () => {
    return render(
      <BrowserRouter>
        <DeletePatient />
      </BrowserRouter>
    );
  };

  test('renderiza o modal de confirmação de exclusão', () => {
    renderDeletePatient();
    
    expect(screen.getByText('Confirm Deletion')).toBeInTheDocument();
    expect(screen.getByText('Are you sure you want to delete this patient?')).toBeInTheDocument();
    expect(screen.getByText('Yes')).toBeInTheDocument();
    expect(screen.getByText('No')).toBeInTheDocument();
  });

  test('navega de volta para a lista quando clica em No', () => {
    renderDeletePatient();
    
    fireEvent.click(screen.getByText('No'));
    expect(mockNavigate).toHaveBeenCalledWith('/patient/list');
  });

  test('exclui paciente com sucesso', async () => {
    patientService.deletePatient.mockResolvedValue({});
    renderDeletePatient();

    await act(async () => {
      fireEvent.click(screen.getByText('Yes'));
    });

    expect(patientService.deletePatient).toHaveBeenCalledWith(mockPatientId);
    expect(screen.getByText('Patient successfully deleted.')).toBeInTheDocument();
  });

  test('redireciona para lista após exclusão bem-sucedida', async () => {
    jest.useFakeTimers();
    patientService.deletePatient.mockResolvedValue({});
    renderDeletePatient();

    await act(async () => {
      fireEvent.click(screen.getByText('Yes'));
    });

    await act(async () => {
      jest.advanceTimersByTime(3000);
    });

    expect(mockNavigate).toHaveBeenCalledWith('/patient/list');
    jest.useRealTimers();
  });

  test('exibe erro quando a exclusão falha', async () => {
    const mockError = new Error('API Error');
    jest.spyOn(window, 'alert').mockImplementation(() => {});
    patientService.deletePatient.mockRejectedValue(mockError);
    
    renderDeletePatient();

    await act(async () => {
      fireEvent.click(screen.getByText('Yes'));
    });

    expect(window.alert).toHaveBeenCalledWith('Error deleting patient.');
  });

  test('verifica se o modal é renderizado corretamente após exclusão', async () => {
    patientService.deletePatient.mockResolvedValue({});
    renderDeletePatient();

    await act(async () => {
      fireEvent.click(screen.getByText('Yes'));
    });

    const modalContent = screen.getByText('Patient successfully deleted.');
    expect(modalContent).toBeInTheDocument();
    expect(screen.queryByText('Yes')).not.toBeInTheDocument();
    expect(screen.queryByText('No')).not.toBeInTheDocument();
  });

  test('verifica se o serviço de exclusão é chamado com o ID correto', async () => {
    patientService.deletePatient.mockResolvedValue({});
    renderDeletePatient();

    await act(async () => {
      fireEvent.click(screen.getByText('Yes'));
    });

    expect(patientService.deletePatient).toHaveBeenCalledWith(mockPatientId);
    expect(patientService.deletePatient).toHaveBeenCalledTimes(1);
  });
});