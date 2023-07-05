import React from 'react';
import { useGetAllUsers } from '../hooks/user-hooks';
const Home = () => {
  const { data: getAllUsers } = useGetAllUsers();
  return <div>home</div>;
};

export default Home;
