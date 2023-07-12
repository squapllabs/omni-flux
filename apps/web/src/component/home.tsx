import React from 'react';
import { useGetAllUsers } from '../hooks/user-hooks';
import Layout from '../layout/layout';
const Home = () => {
  const { data: getAllUsers } = useGetAllUsers();
  console.log('getAllUsers', getAllUsers);

  return <div>home</div>;
};

export default Home;
