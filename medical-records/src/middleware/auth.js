// Author: João Morais

/**
 * Authentication middleware module for handling JWT token validation and authorization
 * This file contains middleware functions to verify user authentication and admin privileges
 * Works in conjunction with the .NET backend authentication system
 */

const jwt = require('jsonwebtoken');

/**
 * Middleware to verify if the request has a valid JWT token
 * Extracts token from Authorization header and validates it
 * Adds decoded user data to request object if valid
 * @param {Object} req - Express request object
 * @param {Object} res - Express response object 
 * @param {Function} next - Express next middleware function
 */
const authMiddleware = async (req, res, next) => {
  try {
    const token = req.headers.authorization?.split(' ')[1];
    
    if (!token) {
      return res.status(401).json({ message: 'Authentication required' });
    }

    // Verify token from .NET backend
    const decoded = jwt.verify(token, process.env.JWT_SECRET);
    req.user = decoded;
    
    next();
  } catch (error) {
    return res.status(403).json({ message: 'Invalid token' });
  }
};

/**
 * Middleware to verify if the authenticated user has admin privileges
 * First validates the JWT token, then checks for admin flag
 * Only allows access if user is authenticated and has admin rights
 * @param {Object} req - Express request object
 * @param {Object} res - Express response object
 * @param {Function} next - Express next middleware function
 */
const isAdmin = async (req, res, next) => {
  try {
    const token = req.headers.authorization?.split(' ')[1];
    
    if (!token) {
      return res.status(401).json({ message: 'Authentication required' });
    }

    const decoded = jwt.verify(token, process.env.JWT_SECRET);
    
    if (!decoded.isAdmin) {
      return res.status(403).json({ message: 'Admin access required' });
    }

    req.user = decoded;
    next();
  } catch (error) {
    return res.status(403).json({ message: 'Invalid token or insufficient permissions' });
  }
};

module.exports = {
  authMiddleware,
  isAdmin
};