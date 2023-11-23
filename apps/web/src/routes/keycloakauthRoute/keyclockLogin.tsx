import React, { useEffect } from 'react';
import keycloak from '../../helper/auth/keycloakconfig';
const KeyclockLogin = () => {
  const login = async () => {
    try {
      await keycloak.init({ onLoad: 'login-required' });
      if (keycloak.authenticated) {
        // Here, the user is authenticated, and you can access onSuccess data
        console.log('User is authenticated');
        console.log('User token:', keycloak.token);
        console.log('User ID:', keycloak.subject);
        // You can access more user-related information from keycloak.userProfile
        console.log('User Profile:', keycloak?.userProfile);
      }
    } catch (error) {
      console.error('Keycloak initialization error:', error);
    }
  };
  useEffect(() => {
    login();
  }, []);
  return keycloak?.init({ onLoad: 'login-required' });
};

export default KeyclockLogin;
