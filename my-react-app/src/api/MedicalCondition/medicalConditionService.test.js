import axios from 'axios';
import medicalConditionService from './medicalConditionService';

jest.mock('axios');

describe('MedicalConditionService', () => {
    let localStorageMock;

    beforeEach(() => {
        localStorageMock = {
            getItem: jest.fn(() => 'fake-token'),
            setItem: jest.fn(),
            clear: jest.fn()
        };
        Object.defineProperty(window, 'localStorage', {
            value: localStorageMock,
            writable: true
        });
        jest.clearAllMocks();
    });

    describe('constructor', () => {
        it('should set correct base URL', () => {
            expect(medicalConditionService.baseUrl).toBe('http://localhost:3001/medical-conditions');
        });
    });

    describe('getAuthToken', () => {
        it('should get token from localStorage', () => {
            const token = medicalConditionService.getAuthToken();
            expect(localStorageMock.getItem).toHaveBeenCalledWith('authToken');
            expect(token).toBe('fake-token');
        });
    });

    describe('addMedicalCondition', () => {
        it('should successfully add medical condition', async () => {
            const mockData = { name: 'Test Condition' };
            const mockResponse = { data: { id: 1, ...mockData } };
            
            axios.post.mockResolvedValue(mockResponse);

            const result = await medicalConditionService.addMedicalCondition(mockData);

            expect(axios.post).toHaveBeenCalledWith(
                'http://localhost:3001/medical-conditions/add-medical-condition',
                mockData,
                {
                    headers: {
                        'Authorization': 'Bearer fake-token',
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockResponse.data);
        });
        
        it('should handle error when adding medical condition', async () => {
            const mockError = new Error('Network error');
            axios.post.mockRejectedValue(mockError);

            await expect(
                medicalConditionService.addMedicalCondition({})
            ).rejects.toThrow('Network error');
        });
    });

    describe('searchMedicalConditions', () => {
        it('should successfully search medical conditions', async () => {
            const mockParams = { query: 'test' };
            const mockResponse = { 
                data: [
                    { id: 1, name: 'Test Condition 1' },
                    { id: 2, name: 'Test Condition 2' }
                ]
            };

            axios.get.mockResolvedValue(mockResponse);

            const result = await medicalConditionService.searchMedicalConditions(mockParams);

            expect(axios.get).toHaveBeenCalledWith(
                'http://localhost:3001/medical-conditions/search',
                {
                    params: mockParams,
                    headers: {
                        'Authorization': 'Bearer fake-token',
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockResponse.data);
        });

        it('should handle error when searching medical conditions', async () => {
            const mockError = new Error('Search failed');
            axios.get.mockRejectedValue(mockError);

            await expect(
                medicalConditionService.searchMedicalConditions({})
            ).rejects.toThrow('Search failed');
        });
    });
});
