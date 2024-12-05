const Specialization = require('../models/Specialization');

class SpecializationRepository {
    async findByName(name) {
        return await Specialization.findOne({ name });
    }

    async create(specialization) {
        const newSpecialization = new Specialization(specialization);
        return await newSpecialization.save();
    }

    async findAll() {
        return await Specialization.find();
    }

    async findById(id) {
        return await Specialization.findById(id);
    }
}

module.exports = new SpecializationRepository(); 