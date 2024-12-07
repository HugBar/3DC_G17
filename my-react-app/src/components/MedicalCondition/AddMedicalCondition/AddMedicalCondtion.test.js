import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import AddMedicalCondition from './AddMedicalCondition';
import medicalConditionService from '../../../api/medicalConditionService';

// Mock do serviço
jest.mock('../../../api/medicalConditionService');

describe('AddMedicalCondition Component', () => {
    beforeEach(() => {
        jest.clearAllMocks();
        jest.spyOn(console, 'error').mockImplementation(() => {});
    });

    afterEach(() => {
        console.error.mockRestore();
    });

    const renderAddMedicalCondition = () => {
        return render(
            <BrowserRouter>
                <AddMedicalCondition />
            </BrowserRouter>
        );
    };

    test('renderiza o formulário corretamente', () => {
        renderAddMedicalCondition();

        expect(screen.getByText('Adicionar Nova Condição Médica')).toBeInTheDocument();
        expect(screen.getByLabelText('Nome da Condição:')).toBeInTheDocument();
        expect(screen.getByLabelText('Severidade:')).toBeInTheDocument();
        expect(screen.getByLabelText('Descrição:')).toBeInTheDocument();
        expect(screen.getByText('Adicionar Condição Médica')).toBeInTheDocument();
    });

    test('adiciona condição médica com sucesso', async () => {
        medicalConditionService.addMedicalCondition.mockResolvedValue({
            message: 'Medical condition added successfully'
        });

        renderAddMedicalCondition();

        fireEvent.change(screen.getByLabelText('Nome da Condição:'), 
            { target: { value: 'Diabetes' } });
        fireEvent.change(screen.getByLabelText('Severidade:'), 
            { target: { value: 'High' } });
        fireEvent.change(screen.getByLabelText('Descrição:'), 
            { target: { value: 'Condição crônica que afeta o metabolismo da glicose' } });

        await act(async () => {
            fireEvent.click(screen.getByText('Adicionar Condição Médica'));
        });

        expect(screen.getByText('Condição médica adicionada com sucesso!')).toBeInTheDocument();
        expect(medicalConditionService.addMedicalCondition).toHaveBeenCalledWith({
            name: 'Diabetes',
            severity: 'High',
            description: 'Condição crônica que afeta o metabolismo da glicose'
        });
    });

    test('exibe mensagem de erro quando a adição falha', async () => {
        const errorMessage = 'Medical condition already exists';
        medicalConditionService.addMedicalCondition.mockRejectedValue({
            response: { 
                data: { 
                    message: errorMessage 
                } 
            }
        });

        renderAddMedicalCondition();

        fireEvent.change(screen.getByLabelText('Nome da Condição:'), 
            { target: { value: 'Diabetes' } });

        await act(async () => {
            fireEvent.click(screen.getByText('Adicionar Condição Médica'));
        });

        expect(screen.getByText(errorMessage)).toBeInTheDocument();
    });

    test('limpa o formulário após adição bem-sucedida', async () => {
        medicalConditionService.addMedicalCondition.mockResolvedValue({});
        
        renderAddMedicalCondition();

        fireEvent.change(screen.getByLabelText('Nome da Condição:'), 
            { target: { value: 'Diabetes' } });
        fireEvent.change(screen.getByLabelText('Descrição:'), 
            { target: { value: 'Descrição teste' } });

        await act(async () => {
            fireEvent.click(screen.getByText('Adicionar Condição Médica'));
        });

        expect(screen.getByLabelText('Nome da Condição:')).toHaveValue('');
        expect(screen.getByLabelText('Severidade:')).toHaveValue('Low');
        expect(screen.getByLabelText('Descrição:')).toHaveValue('');
    });

    test('verifica se todos os campos são obrigatórios', () => {
        renderAddMedicalCondition();

        const submitButton = screen.getByText('Adicionar Condição Médica');
        fireEvent.click(submitButton);

        expect(screen.getByLabelText('Nome da Condição:')).toBeRequired();
        expect(screen.getByLabelText('Severidade:')).toBeRequired();
        expect(screen.getByLabelText('Descrição:')).toBeRequired();
    });
});