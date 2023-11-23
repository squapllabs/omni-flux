import React, { useEffect, useState } from 'react';
import { BrowserRouter, Routes, Route, Router } from 'react-router-dom';
// import { Route, BrowserRouter as Router, Routes } from 'react-router-dom';
import PrivateRoute from './privateRoute';
import EmptyPages from './EmptyPages';
import withkeycloakLayoutAndProtection from './withKeycloakLayoutProduction';
import KeyclockLogin from './keyclockLogin';
import keycloak, { initKeycloak } from '../../helper/auth/keycloakconfig';
// import KeycloakService from '../../helper/auth/keycloakconfig';
import UserPage from './UserPage';
import { useDispatch } from 'react-redux';
import { setToken } from '../../redux/reducer';
import ProtectedRoute from './privateRoute';
import Unauthorized from './Unauthorized';

// const keycloakService = new KeycloakService();
const KeycloakRoutes: React.FC = () => {
  const [keycloakInitialized, setKeycloakInitialized] = useState(false);
  const dispatch = useDispatch();
  useEffect(() => {
    const initializeKeycloak = async () => {
      try {
        const initializedKeycloak = await initKeycloak();
        console.log('initializedKeycloak', initializedKeycloak);
        if (initializedKeycloak) {
          dispatch(
            setToken({ key: 'KeycloakUserData', value: initializedKeycloak })
          );
          setKeycloakInitialized(true);
        }
      } catch (error) {
        setKeycloakInitialized(false);
      }
    };
    initializeKeycloak();
  }, []);
  return (
    <div>
      {/* <BrowserRouter> */}
      <Routes>
        <Route path="/" element={<KeyclockLogin />} />
        <Route
          path="/admin"
          element={
            <PrivateRoute roles={['admin']}>
              <EmptyPages />
            </PrivateRoute>
          }
        />
        <Route
          path="/user-page"
          element={
            <PrivateRoute roles={['user']}>
              <UserPage />
            </PrivateRoute>
          }
        />
      </Routes>
    </div>
    //   );

    // useEffect(() => {
    //   keycloakService.init();
    // }, []);

    // return (
    //   // <Router>
    //   <Routes>
    //     {/* <Route
    //       path="/home"
    //       element={ */}
    //     <ProtectedRoute
    //       path="/home"
    //       keycloak={keycloakService.keycloak}
    //       roles={['user']}
    //       component={UserPage}
    //     />
    //     {/* }
    //     /> */}
    //     {/* <Route

    //       element={ */}
    //     {/* path="/admin" */}
    //     <ProtectedRoute
    //       keycloak={keycloakService.keycloak}
    //       roles={['admin']}
    //       component={EmptyPages}
    //     />
    //     {/* }
    //     /> */}
    //     <Route path="/unauthorized" element={<Unauthorized />} />
    //   </Routes>
    //   // </Router>
  );
};

export default KeycloakRoutes;
