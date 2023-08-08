import React, { useState } from 'react';
import Styles from '../../styles/category.module.scss';
import SideNav from '../ui/sideNav';
import CategoryList from './categoryList';
import SubCategoryList from '../subCategory/subCategoryList';
import SubSubCategoryList from '../subSubCategory/subSubList';
import GstList from '../gst/gstList';
import UomList from '../uom/uomList';
import ClientList from '../client/clientList';
import HsnCodeList from '../hsnCode/hsnCodeList';

/**
 * Category component
 */
const Category = () => {
  const [selectedItem, setSelectedItem] = useState<number>(1);
  const menuItems = [
    {
      id: 1,
      name: 'Category',
    },
    {
      id: 2,
      name: 'Sub Category',
    },
    {
      id: 3,
      name: 'Sub Sub Category',
    },
    
    {
      id: 4,
      name: 'GST',
    },
    {
      id: 5,
      name: 'UOM',
    },
    {
      id: 6,
      name: 'Client',
    },
    {
      id: 7,
      name: 'HSN Code',
    },
  ];
  const handleMenuItemClick = (id: number) => {
    setSelectedItem(id);
  };
  const mainContentComponents: { [key: number]: JSX.Element } = {
    1: <CategoryList />,
    2: <SubCategoryList />,
    3: <SubSubCategoryList />,
    4: <GstList />,
    5: <UomList />,
    6: <ClientList />,
    7: <HsnCodeList />
  };
  return (
    <div className={Styles.container}>
      <div className={Styles.sideContainer}>
        <SideNav
          menuItems={menuItems}
          selectedItem={selectedItem}
          handleMenuItemClick={handleMenuItemClick}
        />
      </div>
      <div className={Styles.mainContainer}>
        {mainContentComponents[selectedItem]}
      </div>
    </div>
  );
};

export default Category;
