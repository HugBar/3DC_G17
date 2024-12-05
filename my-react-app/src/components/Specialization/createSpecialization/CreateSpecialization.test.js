import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import CreateSpecialization from './CreateSpecialization';
import specializationService from '../../../api/specializationService';

jest.mock('../../../api/specializationService', () => ({
  __esModule: true,
  default: {
    addSpecialization: jest.fn()
  }
}));

const mockSpecializationData = {
  name: 'Cardiology',
  description: 'Heart specialist'
};

describe('CreateSpecialization Component', () => {
  beforeAll(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  afterAll(() => {
    console.log.mockRestore();
    console.error.mockRestore();
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  test('renders create specialization form', () => {
    render(<CreateSpecialization />);
    
    expect(screen.getByText('Add New Specialization')).toBeInTheDocument();
    expect(screen.getByLabelText('Specialization Name:')).toBeInTheDocument();
    expect(screen.getByLabelText('Description:')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Add Specialization' })).toBeInTheDocument();
  });

  test('successfully creates a new specialization', async () => {
    specializationService.addSpecialization.mockResolvedValue(mockSpecializationData);
    render(<CreateSpecialization />);

    // Fill in the form
    await act(async () => {
      fireEvent.change(screen.getByLabelText('Specialization Name:'), 
        { target: { value: mockSpecializationData.name } });
      fireEvent.change(screen.getByLabelText('Description:'), 
        { target: { value: mockSpecializationData.description } });
    });

    // Submit form
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: 'Add Specialization' }));
    });

    expect(specializationService.addSpecialization).toHaveBeenCalledWith(mockSpecializationData);
    expect(screen.getByText('Specialization added successfully!')).toBeInTheDocument();
  });

  test('displays error message on failed creation', async () => {
    specializationService.addSpecialization.mockRejectedValue(new Error('API Error'));
    render(<CreateSpecialization />);

    // Fill in the form
    await act(async () => {
      fireEvent.change(screen.getByLabelText('Specialization Name:'), 
        { target: { value: 'Test' } });
      fireEvent.change(screen.getByLabelText('Description:'), 
        { target: { value: 'Test Description' } });
    });

    // Submit form
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: 'Add Specialization' }));
    });

    expect(screen.getByText('Error adding specialization')).toBeInTheDocument();
  });

  test('clears form after successful creation', async () => {
    specializationService.addSpecialization.mockResolvedValue(mockSpecializationData);
    render(<CreateSpecialization />);

    // Fill in the form
    await act(async () => {
      fireEvent.change(screen.getByLabelText('Specialization Name:'), 
        { target: { value: mockSpecializationData.name } });
      fireEvent.change(screen.getByLabelText('Description:'), 
        { target: { value: mockSpecializationData.description } });
    });

    // Submit form
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: 'Add Specialization' }));
    });

    // Check if form is cleared
    expect(screen.getByLabelText('Specialization Name:')).toHaveValue('');
    expect(screen.getByLabelText('Description:')).toHaveValue('');
  });

  test('validates required fields', async () => {
    render(<CreateSpecialization />);

    // Try to submit empty form
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: 'Add Specialization' }));
    });

    // Check if required validation is working
    expect(screen.getByLabelText('Specialization Name:')).toBeInvalid();
    expect(screen.getByLabelText('Description:')).toBeInvalid();
  });

  test('updates form data on input change', () => {
    render(<CreateSpecialization />);

    // Change input values
    fireEvent.change(screen.getByLabelText('Specialization Name:'), 
      { target: { value: 'Test Name' } });
    fireEvent.change(screen.getByLabelText('Description:'), 
      { target: { value: 'Test Description' } });

    // Check if values are updated
    expect(screen.getByLabelText('Specialization Name:')).toHaveValue('Test Name');
    expect(screen.getByLabelText('Description:')).toHaveValue('Test Description');
  });
});