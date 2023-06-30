import React from 'react'
import { useGetAllUsers } from '../hooks/user-hooks';
const Home = () => {
    const { data: getAllUsers } = useGetAllUsers();
    console.log("data", getAllUsers);
    return (
        <div>home</div>
    )
}

export default Home