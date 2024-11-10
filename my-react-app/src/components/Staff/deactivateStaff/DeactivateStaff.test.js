import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import DeactivateStaff from './deactivateStaff';
import staffService from '../../../api/staffService';

jest.mock('../../../api/staffService', () => ({
  __esModule: true,
  default: {
    deactivateStaff: jest.fn(),
    getStaffById: jest.fn()
  }
}));

const mockStaffId = '123';
const mockOnBack = jest.fn();

describe('DeactivateStaff Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  const renderDeactivateStaff = () => {
    return render(
      <BrowserRouter>
        <DeactivateStaff staffId={mockStaffId} onBack={mockOnBack} />
      </BrowserRouter>
    );
  };

  test('renderiza o componente de desativação de staff', () => {
    renderDeactivateStaff();
    
    expect(screen.getByText('Deactivate Staff')).toBeInTheDocument();
    expect(screen.getByText('Deactivate')).toBeInTheDocument();
    expect(screen.getByText('Back')).toBeInTheDocument();
  });

  test('exibe mensagem de erro quando nenhum staff é selecionado', async () => {
    render(<DeactivateStaff staffId={null} onBack={mockOnBack} />);

    await act(async () => {
      fireEvent.click(screen.getByText('Deactivate'));
    });

    expect(screen.getByText('No staff member selected to deactivate.')).toBeInTheDocument();
  });

  test('desativa staff com sucesso', async () => {
    staffService.deactivateStaff.mockResolvedValue({});
    renderDeactivateStaff();

    await act(async () => {
      fireEvent.click(screen.getByText('Deactivate'));
    });

    expect(staffService.deactivateStaff).toHaveBeenCalledWith(mockStaffId);
    expect(screen.getByText('Staff member successfully deactivated.')).toBeInTheDocument();
  });

  test('exibe mensagem de erro quando a desativação falha', async () => {
    staffService.deactivateStaff.mockRejectedValue(new Error('API Error'));
    renderDeactivateStaff();

    await act(async () => {
      fireEvent.click(screen.getByText('Deactivate'));
    });

    expect(screen.getByText('Error deactivating staff member.')).toBeInTheDocument();
  });

  test('chama função onBack quando botão voltar é clicado', () => {
    renderDeactivateStaff();
    
    fireEvent.click(screen.getByText('Back'));
    expect(mockOnBack).toHaveBeenCalled();
  });
});