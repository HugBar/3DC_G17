import React, { useState } from 'react';
import { Modal, Button } from 'react-bootstrap';
import './PrivacyPolicy.css';

const PrivacyPolicy = () => {
    const [show, setShow] = useState(false);

    const handleClose = () => setShow(false);
    const handleShow = () => setShow(true);

    return (
        <>
            <Button 
                variant="info" 
                onClick={handleShow}
                className="privacy-button"
            >
                Privacy Policy
            </Button>

            <Modal 
                show={show} 
                onHide={handleClose}
                size="lg"
                className="privacy-modal"
            >
                <Modal.Header closeButton>
                    <Modal.Title>Privacy Policy</Modal.Title>
                </Modal.Header>
                <Modal.Body>
                    <div className="privacy-content">
                        <h2>1. Information We Collect</h2>
                        
                        <h3>1.1 Personal Information</h3>
                        <p>
                            We collect various types of personal information including your name (first and last), 
                            email address, phone number, date of birth, gender, medical number, and emergency 
                            contact information.
                        </p>

                        <h3>1.2 Medical Information</h3>
                        <p>
                            As part of our healthcare services, we maintain records of your medical conditions 
                            and their severity, allergies and reactions, comprehensive medical history, 
                            appointment records, treatment history, and for our staff members, their medical 
                            specializations.
                        </p>

                        <h3>1.3 System Information</h3>
                        <p>
                            For system security and functionality, we store login credentials, authentication 
                            tokens, system access logs, and session information.
                        </p>

                        <h2>2. How We Use Your Information</h2>
                        <p>
                            We are committed to protecting your privacy and handling your data with transparency. 
                            The information we collect is used exclusively for healthcare service provision and 
                            system functionality. This includes managing your medical appointments, maintaining 
                            accurate health records, and ensuring proper medical care delivery. For staff members, 
                            we use the information to manage schedules, track specializations, and coordinate 
                            patient care effectively. All data processing is conducted in compliance with 
                            applicable healthcare and data protection regulations.
                        </p>

                        <h2>3. Data Protection</h2>
                        <p>
                            We implement robust security measures to protect your information, including 
                            secure authentication mechanisms, encrypted data transmission, and strict access 
                            controls. Our systems are regularly updated and monitored to prevent unauthorized 
                            access. We maintain comprehensive audit trails and ensure all data handling 
                            complies with healthcare security standards. Staff members receive regular 
                            training on data protection protocols.
                        </p>

                        <h2>4. Your Rights</h2>
                        <p>
                            As a user of our services, you have several rights regarding your data. These 
                            include the right to access your medical records, request data corrections, delete 
                            your account, receive your data in a portable format, and opt-out of non-essential 
                            communications.
                        </p>

                        <h2>5. Data Storage and Retention Policies</h2>
                        <p>
                            Patient and staff data are stored in a secure database with specific retention 
                            policies. Adult medical records are kept for 10 years after last activity, while 
                            minor medical records are retained for 10 years after turning 18. Adult personal 
                            data is stored for 3 years after last activity, and minor personal data for 3 
                            years after turning 18.
                        </p>
                        
                        <p>
                            All retention periods comply with General Data Protection Regulation. Data can be 
                            updated or deleted as needed, and email confirmation is sent when sensitive 
                            information is modified. These retention periods were established with our team 
                            following common practices.
                        </p>

                        <h2>6. Contact Information</h2>
                        <p>
                            For privacy-related inquiries, please contact:<br/>
                            Hugo Barros (1220667@isep.ipp.pt)<br/>
                            Matias Vitorino (1220727@isep.ipp.pt)<br/>
                            Pedro Azevedo (1221264@isep.ipp.pt)<br/>
                            João Morais (1211366@isep.ipp.pt)
                        </p>
                    </div>
                </Modal.Body>
                <Modal.Footer>
                    <Button variant="secondary" onClick={handleClose}>
                        Close
                    </Button>
                </Modal.Footer>
            </Modal>
        </>
    );
};

export default PrivacyPolicy;