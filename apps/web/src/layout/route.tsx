import React, { useState } from 'react';
import { Route, Routes, RouteProps } from 'react-router-dom';
import Login from '../component/login';
import Home from '../component/home';
import ForgetPassword from '../component/forgetPassword';
import ResetPassword from '../component/resetPassword/[id]/[token]';
import ProtectedRoute from '../auth/ProtectedRoute';
import Layout from './layout';
const route = () => {
  const [isAuth, setIsAuth] = useState<boolean>(false);
  return (
    <div>
      <Routes>
        <Route path="/" element={<Login setIsAuth={setIsAuth} />} />
        <Route path="/forget-password" element={<ForgetPassword />} />
        <Route path="/reset-password/:id/:token" element={<ResetPassword />} />
        <Route
          path="/home"
          element={
            <ProtectedRoute>
              <Layout>
                <Home />
              </Layout>
            </ProtectedRoute>
          }
        />
      </Routes>
    </div>
  );
};

export default route;
