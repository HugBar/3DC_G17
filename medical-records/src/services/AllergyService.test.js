// Author: Hugo Barros

/**
 * Test suite for AllergyService class
 * Contains unit tests for allergy-related business logic including:
 * - Adding new allergies to the catalog
 * - Searching existing allergies
 * - Error handling and validation
 */

const AllergyService = require('./AllergyService');
const MedicalRecord = require('../models/MedicalRecord');
const Allergy = require('../models/Allergy');
const AllergyDto = require('../dtos/AllergyDto');
const AllergyRepository = require('../repositories/AllergyRepositorie');

// Mock dependencies
jest.mock('../models/MedicalRecord');
jest.mock('../models/Allergy');
jest.mock('../repositories/AllergyRepositorie');

describe('AllergyService', () => {
    // Reset all mocks before each test
    beforeEach(() => {
        jest.clearAllMocks();
    });

    /**
     * Tests for addAllergyModel method
     * Verifies the functionality of adding new allergies to the system catalog
     */
    describe('addAllergyModel', () => {
        const mockAllergyDto = {
            allergen: 'Peanuts',
            severity: 'High',
            diagnosedDate: '2024-03-19',
            notes: 'Test notes'
        };

        /**
         * Test successful addition of a new allergy
         * Should create new allergy when it doesn't already exist
         */
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

        /**
         * Test duplicate allergy handling
         * Should throw error when attempting to add existing allergy
         */
        test('should throw error when allergy already exists', async () => {
            Allergy.findOne.mockResolvedValue({ id: '123', allergen: 'Peanuts' });

            await expect(AllergyService.addAllergyModel(mockAllergyDto))
                .rejects
                .toThrow('Allergy already exists');
        });
    });

    /**
     * Tests for searchAllergies method
     * Verifies the functionality of searching allergies with filters
     */
    describe('searchAllergies', () => {
        const mockSearchDto = {
            allergen: 'Peanuts',
            severity: 'High'
        };

        /**
         * Test successful allergy search
         * Should return properly mapped allergy DTOs when allergies are found
         */
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

        /**
         * Test empty search results
         * Should handle case when no allergies match search criteria
         */
        test('should handle empty search results', async () => {
            AllergyRepository.findByFilters.mockResolvedValue([]);

            const result = await AllergyService.searchAllergies(mockSearchDto);

            expect(result).toEqual([]);
        });

        /**
         * Test error handling
         * Should properly propagate repository errors
         */
        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            AllergyRepository.findByFilters.mockRejectedValue(mockError);

            await expect(AllergyService.searchAllergies(mockSearchDto))
                .rejects
                .toThrow('Database error');
        });
    });
}); 