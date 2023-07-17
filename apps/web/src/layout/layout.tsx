import React, { ReactNode } from 'react';
import Header from './header';
import Navbar from '../component/menu/navbar';

interface LayoutProps {
  children: ReactNode;
}

const Layout: React.FC<LayoutProps> = (props: any) => {
  return (
    <div>

      <Navbar />
      <main>
        {props.children}
      </main>
    </div>
  );
};

export default Layout;
