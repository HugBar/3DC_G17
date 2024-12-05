const MedicalConditionService = require('./MedicalConditionService');
const MedicalRecord = require('../models/MedicalRecord');
const MedicalCondition = require('../models/MedicalCondition');
const MedicalConditionDto = require('../dtos/MedicalConditionDto');
const MedicalConditionRepository = require('../repositories/MedicalConditionRepository');

jest.mock('../models/MedicalRecord');
jest.mock('../models/MedicalCondition');
jest.mock('../repositories/MedicalConditionRepository');

describe('MedicalConditionService', () => {
    beforeEach(() => {
        jest.clearAllMocks();
    });

    describe('addMedicalConditionModel', () => {
        const mockMedicalConditionDto = {
            name: 'Hypertension',
            severity: 'Moderate',
            description: 'High blood pressure condition'
        };

        test('should add new medical condition model when condition does not exist', async () => {
            const mockAddedCondition = { id: '123', ...mockMedicalConditionDto };
            
            MedicalCondition.findOne.mockResolvedValue(null);
            MedicalConditionRepository.addMedicalConditionModel.mockResolvedValue(mockAddedCondition);

            const result = await MedicalConditionService.addMedicalConditionModel(mockMedicalConditionDto);

            expect(MedicalCondition.findOne).toHaveBeenCalledWith({ name: mockMedicalConditionDto.name });
            expect(MedicalConditionRepository.addMedicalConditionModel).toHaveBeenCalledWith(
                expect.any(MedicalCondition)
            );
            expect(result).toEqual(mockAddedCondition);
        });

        test('should throw error when medical condition already exists', async () => {
            MedicalCondition.findOne.mockResolvedValue({ id: '123', name: 'Hypertension' });

            await expect(MedicalConditionService.addMedicalConditionModel(mockMedicalConditionDto))
                .rejects
                .toThrow('Medical condition already exists');
        });

        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            MedicalCondition.findOne.mockRejectedValue(mockError);

            await expect(MedicalConditionService.addMedicalConditionModel(mockMedicalConditionDto))
                .rejects
                .toThrow('Database error');
        });
    });

    describe('searchMedicalConditions', () => {
        const mockSearchDto = {
            name: 'Diabetes',
            severity: 'High'
        };

        test('should return mapped medical conditions when found', async () => {
            const mockConditions = [
                { id: '1', name: 'Diabetes', severity: 'High' },
                { id: '2', name: 'Hypertension', severity: 'Moderate' }
            ];

            MedicalConditionRepository.findByFilters.mockResolvedValue(mockConditions);

            const result = await MedicalConditionService.searchMedicalConditions(mockSearchDto);

            expect(MedicalConditionRepository.findByFilters).toHaveBeenCalledWith(mockSearchDto);
            expect(result).toHaveLength(mockConditions.length);
            expect(result[0]).toBeInstanceOf(MedicalConditionDto);
        });

        test('should handle empty search results', async () => {
            MedicalConditionRepository.findByFilters.mockResolvedValue([]);

            const result = await MedicalConditionService.searchMedicalConditions(mockSearchDto);

            expect(result).toEqual([]);
        });

        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            MedicalConditionRepository.findByFilters.mockRejectedValue(mockError);

            await expect(MedicalConditionService.searchMedicalConditions(mockSearchDto))
                .rejects
                .toThrow('Database error');
        });
    });
});