// Author: João Morais

/**
 * Main application file that sets up the Express server and configures middleware.
 * Handles routing, database connection, error handling, and server initialization.
 * Acts as the entry point for the Medical Records Service API.
 */

const express = require('express');
const cors = require('cors');
const connectDB = require('./config/database'); 
require('dotenv').config();

// Import route handlers
const medicalRecordsRoutes = require('./routes/medicalRecords');
const allergyRoutes = require('./routes/AllergyRoutes');
const specializationRoutes = require('./routes/specialization');
const medicalConditionRoutes = require('./routes/MedicalConditionRoutes');
const surgeryAppointmentRoutes = require('./routes/surgeryAppointments');

const app = express();

/**
 * Initialize database connection
 */
connectDB();

/**
 * Configure middleware
 * - express.json() for parsing JSON request bodies
 * - CORS configuration for frontend access
 */
app.use(express.json());
app.use(cors({
    origin: process.env.FRONTEND_URL,
    credentials: true
}));

/**
 * Register API routes
 * Each route handles a specific domain of the medical records system
 */
app.use('/allergies', allergyRoutes);
app.use('/medical-conditions', medicalConditionRoutes);
app.use('/medical-records', medicalRecordsRoutes);
app.use('/api/specializations', specializationRoutes);
app.use('/api/surgery-appointments', surgeryAppointmentRoutes);

/**
 * Health check endpoint
 * Used for monitoring service availability
 */
app.get('/api/health', (req, res) => {
    res.json({ status: 'Medical Records Service is running' });
});

/**
 * Global error handling middleware
 * Catches all errors and returns appropriate response
 * In development mode, includes error message in response
 */
app.use((err, req, res, next) => {
    console.error(err.stack);
    res.status(500).json({ 
        message: 'Something went wrong!',
        error: process.env.NODE_ENV === 'development' ? err.message : undefined
    });
});

/**
 * Start server on specified port
 * Uses environment variable PORT or defaults to 3001
 */
const PORT = process.env.PORT || 3001;
app.listen(PORT, () => {
    console.log(`Server running on port ${PORT}`);
});

module.exports = app;