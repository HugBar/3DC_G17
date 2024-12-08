import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { BrowserRouter, useNavigate, useLocation } from 'react-router-dom';
import SearchMedicalCondition from './SearchMedicalCondition';
import medicalConditionService from '../../../api/medicalConditionService';

// Mock dos módulos necessários
jest.mock('react-router-dom', () => ({
    ...jest.requireActual('react-router-dom'),
    useNavigate: jest.fn(),
    useLocation: jest.fn()
}));

jest.mock('../../../api/medicalConditionService');

describe('SearchMedicalCondition Component', () => {
    const mockNavigate = jest.fn();
    const mockLocation = { search: '' };

    beforeEach(() => {
        jest.clearAllMocks();
        jest.spyOn(console, 'error').mockImplementation(() => {});
        jest.spyOn(console, 'log').mockImplementation(() => {});
        
        useNavigate.mockImplementation(() => mockNavigate);
        useLocation.mockImplementation(() => mockLocation);
    });

    afterEach(() => {
        console.error.mockRestore();
        console.log.mockRestore();
    });

    const renderSearchMedicalCondition = () => {
        return render(
            <BrowserRouter>
                <SearchMedicalCondition />
            </BrowserRouter>
        );
    };

    test('renderiza o componente de busca corretamente', () => {
        renderSearchMedicalCondition();

        expect(screen.getByText('Search Medical Conditions')).toBeInTheDocument();
        expect(screen.getByPlaceholderText('Condition Name')).toBeInTheDocument();
        expect(screen.getByText('Select Severity')).toBeInTheDocument();
        expect(screen.getByText('Search')).toBeInTheDocument();
        expect(screen.getByText('Clear Filters')).toBeInTheDocument();
    });

    test('realiza busca com filtros preenchidos', async () => {
        const mockConditions = [
            { 
                name: 'Diabetes', 
                severity: 'High',
                description: 'Test description' 
            }
        ];

        medicalConditionService.searchMedicalConditions.mockResolvedValue(mockConditions);
        renderSearchMedicalCondition();

        const nameInput = screen.getByPlaceholderText('Condition Name');
        const severitySelect = screen.getByRole('combobox');
        
        fireEvent.change(nameInput, { target: { value: 'Diabetes' } });
        fireEvent.change(severitySelect, { target: { value: 'High' } });

        await act(async () => {
            fireEvent.click(screen.getByText('Search'));
        });

        expect(medicalConditionService.searchMedicalConditions).toHaveBeenCalledWith({
            name: 'Diabetes',
            severity: 'High'
        });
        expect(screen.getByText('Diabetes')).toBeInTheDocument();
    });

    test('limpa filtros corretamente', () => {
        renderSearchMedicalCondition();

        const nameInput = screen.getByPlaceholderText('Condition Name');
        const severitySelect = screen.getByRole('combobox');

        fireEvent.change(nameInput, { target: { value: 'Test' } });
        fireEvent.change(severitySelect, { target: { value: 'High' } });
        
        fireEvent.click(screen.getByText('Clear Filters'));

        expect(nameInput.value).toBe('');
        expect(severitySelect.value).toBe('');
        expect(mockNavigate).toHaveBeenCalledWith('/medical-conditions/search');
    });

    test('exibe mensagem quando nenhuma condição é encontrada', async () => {
        medicalConditionService.searchMedicalConditions.mockResolvedValue([]);
        renderSearchMedicalCondition();

        await act(async () => {
            fireEvent.click(screen.getByText('Search'));
        });

        expect(screen.getByText('No medical conditions found.')).toBeInTheDocument();
    });

    test('exibe mensagem de erro quando a busca falha', async () => {
        medicalConditionService.searchMedicalConditions.mockRejectedValue({
            response: { status: 500 }
        });
        renderSearchMedicalCondition();

        await act(async () => {
            fireEvent.click(screen.getByText('Search'));
        });

        expect(screen.getByText('Erro ao buscar condições médicas.')).toBeInTheDocument();
    });

    test('abre modal com detalhes ao clicar em uma condição', async () => {
        const mockConditions = [
            { 
                name: 'Diabetes', 
                severity: 'High',
                description: 'Test description' 
            }
        ];

        medicalConditionService.searchMedicalConditions.mockResolvedValue(mockConditions);
        renderSearchMedicalCondition();

        await act(async () => {
            fireEvent.click(screen.getByText('Search'));
        });

        fireEvent.click(screen.getByText('Diabetes'));

        expect(screen.getByText('Condition Details')).toBeInTheDocument();
        expect(screen.getByText('Close')).toBeInTheDocument();
    });

    test('fecha modal ao clicar no botão Close', async () => {
        const mockConditions = [
            { 
                name: 'Diabetes', 
                severity: 'High',
                description: 'Test description' 
            }
        ];

        medicalConditionService.searchMedicalConditions.mockResolvedValue(mockConditions);
        renderSearchMedicalCondition();

        await act(async () => {
            fireEvent.click(screen.getByText('Search'));
        });

        fireEvent.click(screen.getByText('Diabetes'));
        fireEvent.click(screen.getByText('Close'));

        expect(screen.queryByText('Condition Details')).not.toBeInTheDocument();
    });

    test('atualiza URL com os filtros de busca', () => {
        renderSearchMedicalCondition();

        const nameInput = screen.getByPlaceholderText('Condition Name');
        fireEvent.change(nameInput, { target: { value: 'Diabetes' } });

        expect(mockNavigate).toHaveBeenCalledWith(
            '/medical-conditions/search?name=Diabetes',
            { replace: true }
        );
    });
});