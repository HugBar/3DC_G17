const express = require('express');
const cors = require('cors');
const connectDB = require('./config/database');
require('dotenv').config();
const mongoose = require('mongoose');

const app = express();

// Connect to MongoDB
connectDB();

// Middleware
app.use(express.json());
app.use(cors({
    origin: process.env.FRONTEND_URL,
    credentials: true
}));

// Basic route to test connection
app.get('/api/health', (req, res) => {
    res.json({ status: 'Medical Records Service is running' });
});

// Add this route to your app.js
app.get('/api/test', async (req, res) => {
    try {
        // Check MongoDB connection
        if (mongoose.connection.readyState === 1) {
            res.json({ 
                status: 'success',
                message: 'Medical Records Service is connected to MongoDB',
                database: mongoose.connection.db.databaseName
            });
        } else {
            res.status(500).json({ 
                status: 'error',
                message: 'MongoDB not connected',
                readyState: mongoose.connection.readyState
            });
        }
    } catch (error) {
        res.status(500).json({ 
            status: 'error',
            message: error.message 
        });
    }
});

// Error handling middleware
app.use((err, req, res, next) => {
    console.error(err.stack);
    res.status(500).json({ 
        message: 'Something went wrong!',
        error: process.env.NODE_ENV === 'development' ? err.message : undefined
    });
});

const PORT = process.env.PORT || 3001;
app.listen(PORT, () => {
    console.log(`Server running on port ${PORT}`);
});

module.exports = app;