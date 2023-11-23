import React, { useEffect, useState } from 'react';
import keycloak, { initKeycloak } from '../../helper/auth/keycloakconfig';
// import Keycloak, { KeycloakInstance } from 'keycloak-js';
const UserPage = () => {
  //   const [keycloak, setKeycloak] = useState<KeycloakInstance | null>(null);

  //   useEffect(() => {
  //     const initializeKeycloak = async () => {
  //       const keycloakInstance = new Keycloak({
  //         url: 'http://192.168.2.20:8000/',
  //         // url: 'http://localhost:8082/',
  //         realm: 'omni_erp',
  //         clientId: 'restClient',
  //       });

  //       try {
  //         await keycloakInstance.init({
  //           onLoad: 'check-sso',
  //         });

  //         setKeycloak(keycloakInstance);
  //       } catch (error) {
  //         console.error('Error initializing Keycloak:', error);
  //       }
  //     };

  //     initializeKeycloak();
  //   }, []);
  const handleLogout = async () => {
    if (keycloak) {
      const tokenStatus = await keycloak.clearToken();
      console.log('tokenStatus', tokenStatus);

      //   await keycloak.refreshToken();
      const logoutUrl = `${keycloak.createLogoutUrl()}`;
      window.location.href = logoutUrl;
    } else {
      console.error('Keycloak instance not initialized');
    }
  };
  return (
    <div>
      UserPage
      <button onClick={() => handleLogout()}>logout</button>
    </div>
  );
};

export default UserPage;
