const MedicalConditionController = require('./medicalConditionController');
const MedicalConditionService = require('../services/MedicalConditionService');
const MedicalConditionDto = require('../dtos/MedicalConditionDto');
const SearchMedicalConditionDto = require('../dtos/SearchMedicalConditionDto');

jest.mock('../services/MedicalConditionService');

describe('MedicalConditionController', () => {
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
        jest.restoreAllMocks();
    });

    describe('addMedicalCondition', () => {
        beforeEach(() => {
            mockReq = {
                params: { patientId: '123' },
                body: {
                    name: 'Diabetes',
                    severity: 'High',
                    description: 'Chronic condition'
                }
            };
        });

        test('should create and return a new medical condition', async () => {
            const mockMedicalCondition = { id: 1, ...mockReq.body };
            MedicalConditionService.addMedicalCondition.mockResolvedValue(mockMedicalCondition);

            await MedicalConditionController.addMedicalCondition(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(201);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical condition added successfully',
                medicalCondition: mockMedicalCondition
            });
            expect(MedicalConditionService.addMedicalCondition).toHaveBeenCalledWith(
                expect.any(MedicalConditionDto)
            );
        });

        test('should return 404 when medical record not found', async () => {
            MedicalConditionService.addMedicalCondition.mockRejectedValue(new Error('Medical record not found'));

            await MedicalConditionController.addMedicalCondition(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(404);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical record not found'
            });
        });

        test('should return 500 on internal server error', async () => {
            MedicalConditionService.addMedicalCondition.mockRejectedValue(new Error('Database error'));

            await MedicalConditionController.addMedicalCondition(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    describe('addMedicalConditionModel', () => {
        beforeEach(() => {
            mockReq = {
                body: {
                    name: 'Hypertension',
                    severity: 'Moderate',
                    description: 'High blood pressure'
                }
            };
        });

        test('should create and return a new medical condition model', async () => {
            const mockMedicalConditionModel = { id: 1, ...mockReq.body };
            MedicalConditionService.addMedicalConditionModel.mockResolvedValue(mockMedicalConditionModel);

            await MedicalConditionController.addMedicalConditionModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(201);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical condition added successfully',
                medicalCondition: mockMedicalConditionModel
            });
            expect(MedicalConditionService.addMedicalConditionModel).toHaveBeenCalledWith(
                expect.any(MedicalConditionDto)
            );
        });

        test('should return 409 when medical condition already exists', async () => {
            MedicalConditionService.addMedicalConditionModel.mockRejectedValue(new Error('Medical condition already exists'));

            await MedicalConditionController.addMedicalConditionModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(409);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical condition already exists'
            });
        });

        test('should return 500 on internal server error', async () => {
            MedicalConditionService.addMedicalConditionModel.mockRejectedValue(new Error('Database error'));

            await MedicalConditionController.addMedicalConditionModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    describe('searchMedicalConditions', () => {
        test('should return medical conditions with filters', async () => {
            const mockFilters = { name: 'Diabetes', severity: 'High' };
            mockReq = { query: mockFilters };
            const mockConditions = [{ id: 1, ...mockFilters }];

            MedicalConditionService.searchMedicalConditions.mockResolvedValue(mockConditions);

            await MedicalConditionController.searchMedicalConditions(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockConditions);
            expect(MedicalConditionService.searchMedicalConditions).toHaveBeenCalledWith(
                expect.any(SearchMedicalConditionDto)
            );
        });

        test('should return all medical conditions without filters', async () => {
            mockReq = { query: {} };
            const mockConditions = [{ id: 1, name: 'Diabetes' }];

            MedicalConditionService.searchMedicalConditions.mockResolvedValue(mockConditions);

            await MedicalConditionController.searchMedicalConditions(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockConditions);
            expect(MedicalConditionService.searchMedicalConditions).toHaveBeenCalledWith(
                expect.any(SearchMedicalConditionDto)
            );
        });

        test('should handle search errors', async () => {
            mockReq = { query: {} };
            MedicalConditionService.searchMedicalConditions.mockRejectedValue(new Error('Search failed'));

            await MedicalConditionController.searchMedicalConditions(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                error: 'Search failed'
            });
        });
    });
});