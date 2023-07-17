import React from 'react';
import Header from './header';
import Navbar from '../component/menu/navbar';

const Layout: React.FC = (props: any) => {
  return (
    <header>
      <Navbar />
    </header>
  );
};

export default Layout;
