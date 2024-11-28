const express = require('express');
const cors = require('cors');
const connectDB = require('./config/database');
require('dotenv').config();

const medicalRecordsRoutes = require('./routes/medicalRecords');

const app = express();

// Connect to MongoDB
connectDB();

// Middleware
app.use(express.json());
app.use(cors({
    origin: process.env.FRONTEND_URL,
    credentials: true
}));

// Routes
app.use('/api/medical-records', medicalRecordsRoutes);

// Basic health check route
app.get('/api/health', (req, res) => {
    res.json({ status: 'Medical Records Service is running' });
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