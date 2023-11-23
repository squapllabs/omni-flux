import React from 'react';
import keycloak from '../../helper/auth/keycloakconfig';
import Layout from '../../layout/layout';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';

const ProtectedRoute: React.FC = (props: any) => {
  console.log(' props?.roles', keycloak);
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'KeycloakUserData');
  const isAuthorized = props?.roles.some((role: any) =>
    keycloak?.hasRealmRole(role)
  );
  return isAuthorized ? props.children : null;
};

export default ProtectedRoute;

// import { Route, Link } from 'react-router-dom';

// const ProtectedRoute = ({
//   component: Component,
//   roles,
//   keycloak,
//   ...rest
// }: any) => {
//   const isAuthorized =
//     keycloak &&
//     keycloak.authenticated &&
//     roles.some((role: string) => keycloak.hasRealmRole(role));

//   return (
//     <Route
//       {...rest}
//       render={(props: any) =>
//         isAuthorized ? <Component {...props} /> : <Link to="/unauthorized" />
//       }
//     />
//   );
// };

// export default ProtectedRoute;
