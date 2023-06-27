import React from 'react'
import { Route, Routes, Link } from 'react-router-dom';
import Login from '../component/login';
import Home from '../component/home';
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
            </Routes>
        </div>
    )
}

export default route