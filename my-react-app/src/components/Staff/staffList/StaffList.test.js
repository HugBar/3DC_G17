import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import StaffList from './StaffList';
import staffService from '../../../api/staffService';
import userEvent from '@testing-library/user-event';

jest.mock('../../../api/staffService', () => ({
  __esModule: true,
  default: {
    getAllStaff: jest.fn(),
    getStaffById: jest.fn()
  }
}));

const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate,
  useLocation: () => ({
    search: '',
    pathname: '/staff/filter'
  })
}));

const mockStaffData = [
  {
    id: 1,
    firstName: 'John',
    lastName: 'Doe',
    email: 'john@example.com',
    specialization: 'Cardiology',
    phoneNumber: '123456789'
  },
  {
    id: 2,
    firstName: 'Jane',
    lastName: 'Smith',
    email: 'jane@example.com',
    specialization: 'Neurology',
    phoneNumber: '987654321'
  }
];

describe('StaffList Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    staffService.getAllStaff.mockResolvedValue(mockStaffData);
  });

  const renderStaffList = () => {
    return render(
      <BrowserRouter>
        <StaffList onSelectStaff={jest.fn()} />
      </BrowserRouter>
    );
  };

  test('renders staff list component with filters', async () => {
    await act(async () => {
      renderStaffList();
    });
    
    expect(screen.getByPlaceholderText('First Name')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('Last Name')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('Email')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('Specialization')).toBeInTheDocument();
    expect(screen.getByText('Clear Filters')).toBeInTheDocument();
  });

  test('displays staff members after loading', async () => {
    await act(async () => {
      renderStaffList();
    });
    
    expect(await screen.findByText('John Doe')).toBeInTheDocument();
    expect(await screen.findByText('Jane Smith')).toBeInTheDocument();
  });

  test('filters update URL and trigger new search', async () => {
    await act(async () => {
      renderStaffList();
    });
    const user = userEvent.setup();

    const firstNameInput = screen.getByPlaceholderText('First Name');
    await user.type(firstNameInput, 'John');

    expect(mockNavigate).toHaveBeenCalledWith(
      '/staff/filter?firstName=John',
      expect.any(Object)
    );
  });

  test('clear filters resets all inputs and URL', async () => {
    await act(async () => {
      renderStaffList();
    });
    const user = userEvent.setup();

    const firstNameInput = screen.getByPlaceholderText('First Name');
    await user.type(firstNameInput, 'John');
    
    const clearButton = screen.getByText('Clear Filters');
    await user.click(clearButton);

    expect(firstNameInput.value).toBe('');
    expect(mockNavigate).toHaveBeenCalledWith('/staff/filter');
  });

  test('shows error message when API call fails', async () => {
    staffService.getAllStaff.mockRejectedValue(new Error('API Error'));
    
    await act(async () => {
      renderStaffList();
    });

    expect(await screen.findByText('Error fetching staff list.')).toBeInTheDocument();
  });

  test('shows no results message when no staff found', async () => {
    staffService.getAllStaff.mockResolvedValue([]);
    
    await act(async () => {
      renderStaffList();
    });

    expect(await screen.findByText('No staff members found')).toBeInTheDocument();
  });

  test('opens staff details modal when clicking on a staff member', async () => {
    staffService.getStaffById.mockResolvedValue(mockStaffData[0]);
    
    await act(async () => {
      renderStaffList();
    });
    const user = userEvent.setup();

    await screen.findByText('John Doe');
    await user.click(screen.getByText('John Doe'));

    expect(screen.getByText('Staff Details')).toBeInTheDocument();
    expect(screen.getByText('Update Staff')).toBeInTheDocument();
    expect(screen.getByText('Close')).toBeInTheDocument();
  });

  test('closes modal when clicking close button', async () => {
    staffService.getStaffById.mockResolvedValue(mockStaffData[0]);
    
    await act(async () => {
      renderStaffList();
    });
    const user = userEvent.setup();

    await screen.findByText('John Doe');
    await user.click(screen.getByText('John Doe'));
    await user.click(screen.getByText('Close'));

    expect(screen.queryByText('Staff Details')).not.toBeInTheDocument();
  });
});
