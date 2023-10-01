import React from 'react';
import Styles from '../../styles/category.module.scss';
import GstList from '../gst/gstList';
import UomList from '../uom/uomList';
import ClientList from '../client/clientList';
import HsnCodeList from '../hsnCode/hsnCodeList';
import MasterData from './masterData';

interface CategoryProps {
  selectedItem: number;
}

const Category: React.FC<CategoryProps> = ({ selectedItem }) => {
  const mainContentComponents: { [key: number]: JSX.Element } = {
    1: <MasterData />,
    2: <GstList />,
    3: <UomList />,
    4: <ClientList />,
    5: <HsnCodeList />,
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.sideContainer}></div>
      <div className={Styles.mainContainer}>
        {mainContentComponents[selectedItem]}
      </div>
    </div>
  );
};

export default Category;
