const AllergyController = require('./AllergyController');
const AllergyService = require('../services/AllergyService');
const AllergyDto = require('../dtos/AllergyDto');
const AllergySearchDto = require('../dtos/AllergySearchDto');
const CreateAllergyDto = require('../dtos/CreatAllergyDto');

jest.mock('../services/AllergyService');

describe('AllergyController', () => {
    let mockReq;
    let mockRes;

    beforeEach(() => {
        mockRes = {
            status: jest.fn().mockReturnThis(),
            json: jest.fn()
        };
        jest.clearAllMocks();
        jest.spyOn(console, 'error').mockImplementation(() => {});

    });
    afterEach(() => {
        // Restore console.error after each test
        jest.restoreAllMocks();
      });


    describe('addAllergyModel', () => {
        beforeEach(() => {
            mockReq = {
                body: {
                    allergen: 'Peanuts',
                    severity: 'High',
                    diagnosedDate: '2024-03-19',
                    notes: 'Severe reaction'
                }
            };
        });

        test('should create and return a new allergy model', async () => {
            const mockAllergyModel = { id: 1, ...mockReq.body };
            AllergyService.addAllergyModel.mockResolvedValue(mockAllergyModel);

            await AllergyController.addAllergyModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(201);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Allergy added successfully',
                allergy: mockAllergyModel
            });
            expect(AllergyService.addAllergyModel).toHaveBeenCalledWith(
                expect.any(CreateAllergyDto)
            );
        });

        test('should return 409 when allergy already exists', async () => {
            AllergyService.addAllergyModel.mockRejectedValue(new Error('Allergy already exists'));

            await AllergyController.addAllergyModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(409);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Allergy already exists'
            });
        });

        test('should return 500 on internal server error', async () => {
            AllergyService.addAllergyModel.mockRejectedValue(new Error('Database error'));

            await AllergyController.addAllergyModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    describe('searchAllergies', () => {
        test('should return allergies with filters', async () => {
            const mockFilters = { allergen: 'Peanuts', severity: 'High' };
            mockReq = { query: mockFilters };
            const mockAllergies = [{ id: 1, ...mockFilters }];
            
            AllergyService.searchAllergies.mockResolvedValue(mockAllergies);

            await AllergyController.searchAllergies(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockAllergies);
            expect(AllergyService.searchAllergies).toHaveBeenCalledWith(mockFilters);
        });

        test('should return all allergies without filters', async () => {
            mockReq = { query: {} };
            const mockAllergies = [{ id: 1, allergen: 'Peanuts' }];
            
            AllergyService.searchAllergies.mockResolvedValue(mockAllergies);

            await AllergyController.searchAllergies(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockAllergies);
            expect(AllergyService.searchAllergies).toHaveBeenCalledWith({});
        });

        test('should handle search errors', async () => {
            mockReq = { query: {} };
            AllergyService.searchAllergies.mockRejectedValue(new Error('Search failed'));

            await AllergyController.searchAllergies(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                error: 'Search failed'
            });
        });
    });
});