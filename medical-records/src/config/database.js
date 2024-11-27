const mongoose = require('mongoose');
require('dotenv').config();

const connectDB = async () => {
    try {
        const conn = await mongoose.connect(process.env.MONGODB_URI, {
            useNewUrlParser: true,
            useUnifiedTopology: true,
            // For MongoDB 6.0+
            authSource: 'admin'
        });
        
        console.log(`MongoDB Connected: ${conn.connection.host}`);
        
        // Basic error handling
        mongoose.connection.on('error', (err) => {
            console.error('MongoDB connection error:', err);
        });

        mongoose.connection.on('disconnected', () => {
            console.warn('MongoDB disconnected');
        });

    } catch (error) {
        console.error('Error connecting to MongoDB:', error.message);
        if (error.name === 'MongoServerError') {
            console.error('Authentication failed - check username/password');
        }
        process.exit(1);
    }
};

module.exports = connectDB;