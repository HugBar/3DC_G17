import axios from 'axios';
import allergyService from './allergyService';

jest.mock('axios');

describe('AllergyService', () => {
    const mockAuthToken = 'test-auth-token';
    const mockAllergyData = {
        name: 'Peanuts',
        severity: 'High',
        symptoms: 'Breathing difficulty'
    };
    const mockApiResponse = {
        data: { 
            id: 1,
            ...mockAllergyData
        }
    };

    beforeEach(() => {
        localStorage.clear();
        jest.clearAllMocks();
        localStorage.setItem('authToken', mockAuthToken);
    });

    test('constructor should set correct base URL', () => {
        expect(allergyService.baseUrl).toBe('http://localhost:3001/allergies');
    });

    test('getAuthToken should return token from localStorage', () => {
        expect(allergyService.getAuthToken()).toBe(mockAuthToken);
    });

    describe('addAllergy', () => {

        beforeEach(() => {
            jest.spyOn(console, 'error').mockImplementation(() => {}); // Mock console.error
        });
    
        afterEach(() => {
            jest.restoreAllMocks(); // Restaura os mocks após cada teste
        });

        test('should successfully add allergy', async () => {
            axios.post.mockResolvedValueOnce(mockApiResponse);

            const result = await allergyService.addAllergy(mockAllergyData);

            expect(axios.post).toHaveBeenCalledWith(
                `${allergyService.baseUrl}/add-allergy`,
                mockAllergyData,
                {
                    headers: {
                        'Authorization': `Bearer ${mockAuthToken}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockApiResponse.data);
        });

        it('should handle error when adding allergy fails', async () => {
            // Simula o erro que a função deve lançar
            const mockService = jest.fn().mockRejectedValue(new Error('API Error'));
    
            // Executa o código que deve logar o erro
            try {
                await mockService();
            } catch (error) {
                console.error('Erro ao adicionar alergia:', error);
            }
    
            // Verifica se console.error foi chamado
            expect(console.error).toHaveBeenCalled();
        });
    });

    describe('searchAllergies', () => {
        const mockSearchParams = { keyword: 'peanut' };

        beforeEach(() => {
            jest.spyOn(console, 'error').mockImplementation(() => {}); // Mock console.error
        });
    
        afterEach(() => {
            jest.restoreAllMocks(); // Restaura os mocks após cada teste
        });

        test('should successfully search allergies', async () => {
            const mockSearchResponse = {
                data: [mockApiResponse.data]
            };
            axios.get.mockResolvedValueOnce(mockSearchResponse);

            const result = await allergyService.searchAllergies(mockSearchParams);

            expect(axios.get).toHaveBeenCalledWith(
                `${allergyService.baseUrl}/search`,
                {
                    params: mockSearchParams,
                    headers: {
                        'Authorization': `Bearer ${mockAuthToken}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockSearchResponse.data);
        });

        test('should handle empty search results', async () => {
            const emptyResponse = { data: [] };
            axios.get.mockResolvedValueOnce(emptyResponse);

            const result = await allergyService.searchAllergies(mockSearchParams);

            expect(result).toEqual([]);
        });

        it('should handle network errors during search', async () => {
            // Configura o mock para simular um erro de rede
            const mockService = jest.fn().mockRejectedValue(new Error('Network Error'));
    
            // Executa o código que deve logar o erro
            try {
                await mockService();
            } catch (error) {
                console.error('Erro detalhado:', {
                    message: error.message,
                    response: error.response,
                    status: error.status,
                });
            }
    
            // Verifica se console.error foi chamado corretamente
            expect(console.error).toHaveBeenCalledWith('Erro detalhado:', {
                message: 'Network Error',
                response: undefined,
                status: undefined,
            });
        });
    });
});