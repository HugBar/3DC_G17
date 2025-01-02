const MedicalRecord = require('../models/MedicalRecord');
const Allergy = require('../models/Allergy');
const AllergyDto = require('../dtos/AllergyDto');
const AlergyRepository = require('../repositories/AllergyRepositorie');


class AllergyService {
    async addAllergy(allergyDto) {
        try {
            const medicalRecord = await MedicalRecord.findOne({ patientId });
            console.log(medicalRecord);
            
            if (!medicalRecord) {
                throw new Error('Medical record not found');
            }

            const newAllergy = {
                allergen: allergyDto.allergen,
                severity: allergyDto.severity,
                description: allergyDto.desription
            };

            const allergy = await AlergyRepository.addAllergy(newAllergy);        
            
            return allergy;
        } catch (error) {
            throw error;
        }
    }

    async addAllergyModel(allergyDto) {
        try {
            const allergy = await Allergy.findOne({ allergen: allergyDto.allergen });
            if (allergy) {
                throw new Error('Allergy already exists');
            }

            const newAllergy = new Allergy({
                allergen: allergyDto.allergen,
                severity: allergyDto.severity,
                description: allergyDto.description
                
            });

            console.log(newAllergy);

            const addedAllergy = await AlergyRepository.addAllergyModel(newAllergy);

            return addedAllergy;

        } catch (error) {
            throw error;
        }
    }

    // Get all allergies from allergy model
    async searchAllergies(allergySearchDto) {
        try {
           

            const allergies = await AlergyRepository.findByFilters(allergySearchDto);
            return allergies.map(allergy => new AllergyDto(
            allergy.id,
            allergy.allergen,
            allergy.severity,
            allergy.description
        ));
        } catch (error) {
            throw error;
        }
    }

    async getAllAllergies() {
        try {
            const allergies = await AlergyRepository.getAllAllergiesWithDetails();
            if (!allergies || allergies.length === 0) {
                return [];
            }
            return allergies;
        } catch (error) {
            throw error;
        }
    }

    async deleteAllergy(allergyId) {
        try {
            const allergy = await AlergyRepository.deleteAllergy(allergyId);
            return allergy;
        } catch (error) {
            throw error;
        }
    }

    async updateAllergy(id, allergyDto) {
        try {
            const allergyIdExists = await AlergyRepository.findById(id);
            console.log(allergyIdExists);
            if (!allergyIdExists) {
                throw new Error('Allergy not found');
            }
        
            
            const allergy = await AlergyRepository.updateAllergy(id, allergyDto);

            console.log(allergy);

            const updatedAllergy = new AllergyDto(
                allergy.id,
                allergy.allergen,
                allergy.severity,
                allergy.description
            );

            return updatedAllergy;
        } catch (error) {
            throw error;
        }
    }
}

module.exports = new AllergyService();
