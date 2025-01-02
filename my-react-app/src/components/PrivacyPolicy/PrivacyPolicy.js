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
                        <ul>
                            <li>Name (first and last)</li>
                            <li>Email address</li>
                            <li>Phone number</li>
                            <li>Date of birth</li>
                            <li>Gender</li>
                            <li>Medical number</li>
                            <li>Emergency contact information</li>
                        </ul>

                        <h3>1.2 Medical Information</h3>
                        <ul>
                            <li>Medical conditions and their severity</li>
                            <li>Allergies and reactions</li>
                            <li>Medical history</li>
                            <li>Appointment records</li>
                            <li>Treatment history</li>
                            <li>Medical specializations (for staff)</li>
                        </ul>

                        <h3>1.3 System Information</h3>
                        <ul>
                            <li>Login credentials</li>
                            <li>Authentication tokens</li>
                            <li>System access logs</li>
                            <li>Session information</li>
                        </ul>

                        <h2>2. How We Use Your Information</h2>
                        <h3>2.1 Primary Uses</h3>
                        <ul>
                            <li>Providing medical care and treatment</li>
                            <li>Managing patient appointments</li>
                            <li>Maintaining medical records</li>
                            <li>Emergency contact purposes</li>
                            <li>Staff management and specialization tracking</li>
                        </ul>

                        <h2>3. Data Protection</h2>
                        <p>We implement various security measures to protect your information:</p>
                        <ul>
                            <li>Secure JWT authentication</li>
                            <li>Encrypted data transmission</li>
                            <li>Role-based access control</li>
                            <li>Regular security audits</li>
                            <li>Automated session management</li>
                        </ul>

                        <h2>4. Your Rights</h2>
                        <p>You have the right to:</p>
                        <ul>
                            <li>Access your medical records</li>
                            <li>Request data corrections</li>
                            <li>Delete your account</li>
                            <li>Receive your data in a portable format</li>
                            <li>Opt-out of non-essential communications</li>
                        </ul>

                        <h2>5. Data Storage and Retention Policies</h2>
                        <p>Patient and staff data are stored in a secure database with the following retention policies:</p>
                        <ul>
                            <li>Adult medical records: 10 years after last activity</li>
                            <li>Minor medical records: 10 years after turning 18</li>
                            <li>Adult personal data: 3 years after last activity</li>
                            <li>Minor personal data: 3 years after turning 18</li>
                        </ul>

                        <p>Additional security measures:</p>
                        <ul>
                            <li>All retention periods comply with General Data Protection Regulation</li>
                            <li>Data can be updated or deleted as needed</li>
                            <li>Email confirmation is sent when sensitive information is modified</li>
                            <li>Retention periods were established with our team following common practices</li>
                        </ul>
                        <h2>6. Contact Information</h2>
                        <p>For privacy-related inquiries:</p>
                        <ul> 
                            <li>Hugo Barros 1220667@isep.ipp.pt</li>
                            <li>Matias Vitorino 1220727@isep.ipp.pt</li>
                            <li>Pedro Azevedo 1221264@isep.ipp.pt</li>
                            <li>João Morais 1211366@isep.ipp.pt</li>
                        </ul>

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