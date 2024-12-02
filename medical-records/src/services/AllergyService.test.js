const AllergyService = require('./AllergyService');
const MedicalRecord = require('../models/MedicalRecord');
const Allergy = require('../models/Allergy');
const AllergyDto = require('../dtos/AllergyDto');
const AllergyRepository = require('../repositories/AllergyRepositorie');

jest.mock('../models/MedicalRecord');
jest.mock('../models/Allergy');
jest.mock('../repositories/AllergyRepositorie');

describe('AllergyService', () => {
    beforeEach(() => {
        jest.clearAllMocks();
    });

    describe('addAllergyModel', () => {
        const mockAllergyDto = {
            allergen: 'Peanuts',
            severity: 'High',
            diagnosedDate: '2024-03-19',
            notes: 'Test notes'
        };

        test('should add new allergy model when allergy does not exist', async () => {
            const mockAddedAllergy = { id: '123', ...mockAllergyDto };
            
            Allergy.findOne.mockResolvedValue(null);
            AllergyRepository.addAllergyModel.mockResolvedValue(mockAddedAllergy);

            const result = await AllergyService.addAllergyModel(mockAllergyDto);

            expect(Allergy.findOne).toHaveBeenCalledWith({ allergen: mockAllergyDto.allergen });
            expect(AllergyRepository.addAllergyModel).toHaveBeenCalledWith(
                expect.any(Allergy)
            );
            expect(result).toEqual(mockAddedAllergy);
        });

        test('should throw error when allergy already exists', async () => {
            Allergy.findOne.mockResolvedValue({ id: '123', allergen: 'Peanuts' });

            await expect(AllergyService.addAllergyModel(mockAllergyDto))
                .rejects
                .toThrow('Allergy already exists');
        });
    });

    describe('searchAllergies', () => {
        const mockSearchDto = {
            allergen: 'Peanuts',
            severity: 'High'
        };

        test('should return mapped allergies when found', async () => {
            const mockAllergies = [
                { id: '1', allergen: 'Peanuts', severity: 'High' },
                { id: '2', allergen: 'Dairy', severity: 'Medium' }
            ];

            AllergyRepository.findByFilters.mockResolvedValue(mockAllergies);

            const result = await AllergyService.searchAllergies(mockSearchDto);

            expect(AllergyRepository.findByFilters).toHaveBeenCalledWith(mockSearchDto);
            expect(result).toHaveLength(mockAllergies.length);
            expect(result[0]).toBeInstanceOf(AllergyDto);
        });

        test('should handle empty search results', async () => {
            AllergyRepository.findByFilters.mockResolvedValue([]);

            const result = await AllergyService.searchAllergies(mockSearchDto);

            expect(result).toEqual([]);
        });

        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            AllergyRepository.findByFilters.mockRejectedValue(mockError);

            await expect(AllergyService.searchAllergies(mockSearchDto))
                .rejects
                .toThrow('Database error');
        });
    });
}); 