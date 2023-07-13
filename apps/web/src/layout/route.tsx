import React, { useState } from 'react';
import { Route, Routes, RouteProps } from 'react-router-dom';
import Login from '../component/login';
import Home from '../component/home';
import ForgetPassword from '../component/forgetPassword';
import ResetPassword from '../component/resetPassword/[id]/[token]';
import ProtectedRoute from '../auth/ProtectedRoute';
import Layout from './layout';
import UserCreate from '../component/users/userCreate';
import UserEdit from '../component/users/userEdit';
import UserList from '../component/users/userList';
import UserInformation from '../component/users/userInformation';
import GstList from '../component/gst/gstList';
const route = () => {
  const [isAuth, setIsAuth] = useState<boolean>(false);
  return (
    <div>
      <Routes>
        <Route path="/" element={<Login setIsAuth={setIsAuth} />} />
        <Route path="/forget-password" element={<ForgetPassword />} />
        <Route path="/reset-password/:id/:token" element={<ResetPassword />} />
        <Route
          path="/userList"
          element={
            <ProtectedRoute>
              <Layout />
              <UserList />
            </ProtectedRoute>
          }
        />
        <Route
          path="/userInfo/:id"
          element={
            <ProtectedRoute>
              <Layout />
              <UserInformation />
            </ProtectedRoute>
          }
        />
        <Route
          path="/home"
          element={
            <ProtectedRoute>
              <Layout />
              <Home />
            </ProtectedRoute>
          }
        />
        <Route
          path="/user-create"
          element={
            <ProtectedRoute>
              <Layout />
              <UserCreate />
            </ProtectedRoute>
          }
        />
        <Route
          path="/user-edit/:id"
          element={
            <ProtectedRoute>
              <Layout />
              <UserEdit />
            </ProtectedRoute>
          }
        />
        <Route
          path="/gst-list"
          element={
            <ProtectedRoute>
              <Layout />
              <GstList />
            </ProtectedRoute>
          }
        />
      </Routes>
    </div>
  );
};

export default route;
