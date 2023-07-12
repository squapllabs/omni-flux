import React from 'react';
import { useGetAllUsers } from '../hooks/user-hooks';
import Styles from '../styles/home.module.scss';

const Home = () => {
  const { data: getAllUsers } = useGetAllUsers();
  console.log('getAllUsers', getAllUsers);

  return (
    <div className={Styles.container}>
      UNDER CONSTRUCTION
    </div>

  );
};

export default Home;
