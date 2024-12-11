const express = require('express');
const cors = require('cors');
const connectDB = require('./config/database');
require('dotenv').config();

const medicalRecordsRoutes = require('./routes/medicalRecords');
const allergyRoutes = require('./routes/AllergyRoutes');
const specializationRoutes = require('./routes/specialization');
const medicalConditionRoutes = require('./routes/MedicalConditionRoutes');
const surgeryAppointmentRoutes = require('./routes/surgeryAppointments');

const app = express();

// Connect to MongoDB
connectDB();

// Middleware
app.use(express.json());
app.use(cors({
    origin: process.env.FRONTEND_URL,
    credentials: true
}));

//Routes
app.use('/allergies', allergyRoutes);
app.use('/medical-conditions', medicalConditionRoutes);
app.use('/medical-records', medicalRecordsRoutes);
app.use('/api/specializations', specializationRoutes);
app.use('/api/surgery-appointments', surgeryAppointmentRoutes);

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