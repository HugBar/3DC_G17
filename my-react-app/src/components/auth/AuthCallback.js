import { useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { useAuth } from '../../context/AuthContext';
import { handleGoogleCallback } from '../../api/auth';

const AuthCallback = ({ onLoginSuccess }) => {
  const navigate = useNavigate();
  const { login, isPatient } = useAuth();

  useEffect(() => {
    const handleAuth = async () => {
      const params = new URLSearchParams(window.location.search);
      const token = params.get('token');
      const code = params.get('code');
      
      if (!token && !code) {
        navigate('/login', { replace: true });
        return;
      }

      try {
        const finalToken = await handleGoogleCallback(token || code);
        login(finalToken);
        
        if (isPatient) {
          onLoginSuccess();
        }
        navigate('/', { replace: true });
      } catch (error) {
        console.error('Authentication failed:', error);
        navigate('/login', { replace: true });
      }
    };

    handleAuth();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return null;
};

export default AuthCallback;