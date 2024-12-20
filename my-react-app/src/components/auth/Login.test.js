// src/components/Login/Login.test.js
import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { useAuth } from '../../context/AuthContext';
import Login from './Login';
import { login as loginApi } from '../../api/auth';
import { useNavigate } from 'react-router-dom';

// Mock useAuth and login API
jest.mock('../../context/AuthContext');
jest.mock('../../api/auth');
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: jest.fn(),
}));

describe('Login Component', () => {
  const mockNavigate = jest.fn();
  const mockLogin = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
    useAuth.mockReturnValue({ login: mockLogin });
    useNavigate.mockReturnValue(mockNavigate);
  });

  test('renders login form with email and password fields', () => {
    render(
      <MemoryRouter>
        <Login />
      </MemoryRouter>
    );

    expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/password/i)).toBeInTheDocument();
    expect(screen.getByRole('button', { 
      name: 'Login',
      type: 'submit',
      className: 'login-button'
    })).toBeInTheDocument();
  });

  test('shows error message on invalid login attempt', async () => {
    const consoleErrorMock = jest.spyOn(console, 'error').mockImplementation(() => {});
    loginApi.mockRejectedValue(new Error('Invalid credentials'));

    render(
      <MemoryRouter>
        <Login />
      </MemoryRouter>
    );

    fireEvent.change(screen.getByLabelText(/email/i), {
      target: { value: 'test@example.com' },
    });
    fireEvent.change(screen.getByLabelText(/password/i), {
      target: { value: 'wrongpassword' },
    });

    const submitButton = screen.getByRole('button', { 
      name: 'Login',
      type: 'submit',
      className: 'login-button'
    });
    fireEvent.click(submitButton);

    const errorMessage = await screen.findByText('Error logging in.');
    expect(errorMessage).toBeInTheDocument();

    consoleErrorMock.mockRestore();
  });

  test('navigates to home on successful login', async () => {
    loginApi.mockResolvedValue({ token: 'fake-jwt-token' });

    render(
      <MemoryRouter>
        <Login />
      </MemoryRouter>
    );

    fireEvent.change(screen.getByLabelText(/email/i), {
      target: { value: 'test@example.com' },
    });
    fireEvent.change(screen.getByLabelText(/password/i), {
      target: { value: 'correctpassword' },
    });

    const submitButton = screen.getByRole('button', { 
      name: 'Login',
      type: 'submit',
      className: 'login-button'
    });
    fireEvent.click(submitButton);

    await waitFor(() => {
      expect(mockLogin).toHaveBeenCalledWith({ token: 'fake-jwt-token' });
      expect(mockNavigate).toHaveBeenCalledWith('/');
    });
  });
});
