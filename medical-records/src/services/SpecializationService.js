const SpecializationRepository = require('../repositories/SpecializationRepository');

class SpecializationService {
    static async addSpecialization(specializationDto) {
        try {
            const existingSpecialization = await SpecializationRepository.findByName(specializationDto.name);
            if (existingSpecialization) {
                throw new Error('Specialization already exists');
            }

            return await SpecializationRepository.create({
                name: specializationDto.name,
                description: specializationDto.description
            });
        } catch (error) {
            throw error;
        }
    }

    static async getAllSpecializations() {
        try {
            return await SpecializationRepository.findAll();
        } catch (error) {
            throw error;
        }
    }

    static async getSpecializationById(id) {
        try {
            const specialization = await SpecializationRepository.findById(id);
            if (!specialization) {
                throw new Error('Specialization not found');
            }
            return specialization;
        } catch (error) {
            throw error;
        }
    }
}

module.exports = SpecializationService; 