// src/hooks/useFormValidation.js
import { useState } from 'react';

const useFormValidation = () => {
  const [errors, setErrors] = useState({});

  const validate = (values) => {
    const newErrors = {};
    if (!values.firstName?.trim()) newErrors.firstName = 'First name is required';
    if (!values.lastName?.trim()) newErrors.lastName = 'Last name is required';
    if (!values.email?.trim()) {
      newErrors.email = 'Email is required';
    } else if (!/\S+@\S+\.\S+/.test(values.email)) {
      newErrors.email = 'Email is invalid';
    }
    if (!values.phoneNumber.trim()) newErrors.phoneNumber = 'Phone number is required';
    if (!values.specialization.trim()) newErrors.specialization = 'Specialization is required';

    values.availabilitySlots.forEach((slot, index) => {
      if (!slot.startTime) newErrors[`availabilitySlots.${index}.startTime`] = 'Start time is required';
      if (!slot.endTime) newErrors[`availabilitySlots.${index}.endTime`] = 'End time is required';
    });

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  return { errors, validate };
};

export default useFormValidation;
