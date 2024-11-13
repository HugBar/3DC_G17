import React from 'react';
import { render, screen, waitFor, fireEvent, within } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import OperationRequestList from './OperationRequestList';
import operationRequestService from '../../../api/operationRequestService';
import userEvent from '@testing-library/user-event';
import '@testing-library/jest-dom';

// Mock the operationRequestService
jest.mock('../../../api/operationRequestService', () => ({
  __esModule: true,
  default: {
    getAllOperationRequests: jest.fn()
  }
}));

// Mock navigate and location
const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate,
  useLocation: () => ({
    search: '',
    pathname: '/operationrequest/filter'
  })
}));

const mockOperationRequests = [
  {
    id: 'OP-123',
    doctorLicenseNumber: 'LIC-123',
    patientMedicalNumber: 'PAT-456',
    priority: 'urgent',
    deadline: '2024-03-20T10:00:00',
    isScheduled: false,
    operationTypeId: 'OT-789'
  },
  {
    id: 'OP-456',
    doctorLicenseNumber: 'LIC-789',
    patientMedicalNumber: 'PAT-789',
    priority: 'elective',
    deadline: '2024-03-25T14:00:00',
    isScheduled: true,
    operationTypeId: 'OT-012'
  }
];

describe('OperationRequestList Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    operationRequestService.getAllOperationRequests.mockResolvedValue(mockOperationRequests);
  });

  const renderOperationRequestList = () => {
    return render(
      <BrowserRouter>
        <OperationRequestList />
      </BrowserRouter>
    );
  };

  test('renders operation request list component with filters', async () => {
    renderOperationRequestList();

    await waitFor(() => {
      expect(screen.getByPlaceholderText('Doctor License Number')).toBeInTheDocument();
      expect(screen.getByPlaceholderText('Patient Medical Number')).toBeInTheDocument();
      expect(screen.getByPlaceholderText('Priority')).toBeInTheDocument();
      expect(screen.getByText('Clear Filters')).toBeInTheDocument();
    });
  });

  test('displays operation requests after loading', async () => {
    renderOperationRequestList();

    await waitFor(() => {
      expect(screen.getByText(/Request ID: OP-123/)).toBeInTheDocument();
      expect(screen.getByText(/Request ID: OP-456/)).toBeInTheDocument();
      expect(screen.getByText(/LIC-123/)).toBeInTheDocument();
      expect(screen.getByText(/PAT-456/)).toBeInTheDocument();
    });
  });

  test('filters update URL and trigger new search', async () => {
    renderOperationRequestList();

    const user = userEvent.setup();

    await waitFor(() => {
      expect(screen.getByPlaceholderText('Doctor License Number')).toBeInTheDocument();
    });

    const doctorInput = screen.getByPlaceholderText('Doctor License Number');
    await user.type(doctorInput, 'LIC-123');

    await waitFor(() => {
      expect(mockNavigate).toHaveBeenCalledWith(
        '/operationrequest/filter?doctorLicenseNumber=LIC-123',
        expect.any(Object)
      );
    });
  });

  test('clear filters resets all inputs and URL', async () => {
    renderOperationRequestList();
    const user = userEvent.setup();

    await waitFor(() => {
      expect(screen.getByPlaceholderText('Doctor License Number')).toBeInTheDocument();
    });

    const doctorInput = screen.getByPlaceholderText('Doctor License Number');
    await user.type(doctorInput, 'LIC-123');

    const clearButton = screen.getByText('Clear Filters');
    await user.click(clearButton);

    await waitFor(() => {
      expect(doctorInput).toHaveValue('');
      expect(mockNavigate).toHaveBeenLastCalledWith('/operationrequest/filter');
    });
  });

  test('shows no results message when no requests found', async () => {
    operationRequestService.getAllOperationRequests.mockResolvedValueOnce([]);

    renderOperationRequestList();

    await waitFor(() => {
      expect(screen.getByText('No operation requests found')).toBeInTheDocument();
    });
  });

  test('displays operation request details when clicking on a request', async () => {
    renderOperationRequestList();
    const user = userEvent.setup();

    const requestCard = await screen.findByText('Request ID: OP-123');
    await user.click(requestCard);

    await waitFor(() => {
      const modalContent = screen.getByTestId('modal-content');
      expect(modalContent).toBeInTheDocument();
      
      within(modalContent).getByText('Operation Request Details');
      within(modalContent).getByText('urgent');
      within(modalContent).getByText('Not Scheduled');
    });
  });

  test('closes details modal when clicking back button', async () => {
    renderOperationRequestList();

    const user = userEvent.setup();

    await waitFor(() => {
      expect(screen.getByText(/Request ID: OP-123/)).toBeInTheDocument();
    });

    const requestCard = screen.getByText(/Request ID: OP-123/);
    await user.click(requestCard);

    await waitFor(() => {
      expect(screen.getByText('Back to List')).toBeInTheDocument();
    });

    const backButton = screen.getByText('Back to List');
    await user.click(backButton);

    await waitFor(() => {
      expect(screen.queryByText('Operation Request Details')).not.toBeInTheDocument();
    });
  });
});