// src/hooks/usePatientFormValidation.js
import { useState } from 'react';

const usePatientFormValidation = () => {
  const [errors, setErrors] = useState({});

  const validate = (values) => {
    const newErrors = {};

    // Basic information validation
    if (!values.firstName?.trim()) {
      newErrors.firstName = 'First name is required';
    }

    if (!values.lastName?.trim()) {
      newErrors.lastName = 'Last name is required';
    }

    // Email validation
    if (!values.email?.trim()) {
      newErrors.email = 'Email is required';
    } else if (!/\S+@\S+\.\S+/.test(values.email)) {
      newErrors.email = 'Email is invalid';
    }

    // Phone number validation
    if (!values.phoneNumber?.trim()) {
      newErrors.phoneNumber = 'Phone number is required';
    } else if (!/^\d{9}$/.test(values.phoneNumber.trim())) {
      newErrors.phoneNumber = 'Phone number must be exactly 9 digits';
    }

    // Date of birth validation
    if (!values.dateOfBirth?.trim()) {
      newErrors.dateOfBirth = 'Date of birth is required';
    }

    // Gender validation
    if (!values.gender?.trim()) {
      newErrors.gender = 'Gender is required';
    }

    // Contact info validation
    if (!values.contactInfo?.trim()) {
      newErrors.contactInfo = 'Contact information is required';
    }

    // Emergency contact validation
    if (!values.emergencyContact?.trim()) {
      newErrors.emergencyContact = 'Emergency contact is required';
    }

    // Medical Number validation (if applicable)
    if (values.medicalNr && !/^[A-Z0-9]{8}$/.test(values.medicalNr.trim())) {
      newErrors.medicalNr = 'Medical number must be 8 characters long and contain only uppercase letters and numbers';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  return { errors, validate };
};

export default usePatientFormValidation;
