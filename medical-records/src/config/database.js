// Author: João Morais

/**
 * This module handles the MongoDB database connection configuration.
 * It provides functionality to establish and monitor the connection to MongoDB,
 * including error handling and connection status monitoring.
 * The connection details are loaded from environment variables for security.
 */

const mongoose = require('mongoose');
require('dotenv').config();

/**
 * Establishes connection to MongoDB database
 * Configures connection options and sets up event listeners for monitoring
 * @returns {Promise<void>} Resolves when connection is established
 * @throws {Error} If connection fails
 */
const connectDB = async () => {
    try {
        // Establish connection with MongoDB using environment variables
        const conn = await mongoose.connect(process.env.MONGODB_URI, {
            useNewUrlParser: true,
            useUnifiedTopology: true,
            // For MongoDB 6.0+
            authSource: 'admin'
        });
        
        console.log(`MongoDB Connected: ${conn.connection.host}`);
        
        // Set up error event listener to monitor connection errors
        mongoose.connection.on('error', (err) => {
            console.error('MongoDB connection error:', err);
        });

        // Set up disconnection event listener to monitor connection status
        mongoose.connection.on('disconnected', () => {
            console.warn('MongoDB disconnected');
        });

    } catch (error) {
        // Handle connection errors with detailed logging
        console.error('Error connecting to MongoDB:', error.message);
        if (error.name === 'MongoServerError') {
            console.error('Authentication failed - check username/password');
        }
        process.exit(1);
    }
};

module.exports = connectDB;