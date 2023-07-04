import React from 'react'
import { Route, Routes, Link } from 'react-router-dom';
import Login from '../component/login';
import Home from '../component/home';
import ForgetPassword from '../component/forgetPassword';
const route = () => {
    return (
        <div>
            <Routes>
                <Route
                    path="/"
                    element={<Login />}
                />
                <Route
                    path="/home"
                    element={<Home />}
                />
                <Route
                    path="/forget-password"
                    element={<ForgetPassword />}
                />
            </Routes>
        </div>
    )
}

export default route