import Keycloak, { KeycloakInstance } from 'keycloak-js';

const keycloakConfig = {
  url: 'http://192.168.2.20:8000/',
  // url: 'http://localhost:8082/',
  // url: 'http://localhost:8084/auth/',
  realm: 'omni_erp',
  clientId: 'restClient',
};
const keycloak = new Keycloak(keycloakConfig);
export const initKeycloak = async (): Promise<KeycloakInstance> => {
  try {
    const initialized = await keycloak.init({ onLoad: 'login-required' });
    console.log('initialized', initialized);
    if (initialized) {
      await keycloak.loadUserInfo();
      return keycloak;
    } else {
      throw new Error('User authentication failed');
    }
  } catch (error) {
    console.error('Keycloak initialization error:', error);
    throw error;
  }
};
export default keycloak;

// class KeycloakService {
//   keycloak: Keycloak.KeycloakInstance;

//   constructor() {
//     this.keycloak = new Keycloak(keycloakConfig);
//   }

//   init = async () => {
//     await this.keycloak.init({
//       onLoad: 'login-required',
//     });
//   };

//   login = () => this.keycloak.login();

//   logout = () => this.keycloak.logout();

//   getToken = () => this.keycloak.token;

//   getUser = () => this.keycloak.user;

//   isAuthenticated = () => this.keycloak.authenticated;
// }

// export default KeycloakService;
