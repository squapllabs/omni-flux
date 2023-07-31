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
      name: 'Team',
    },
    {
      id: 5,
      name: 'Plan',
    },
    {
      id: 6,
      name: 'GST',
    },
    {
      id: 7,
      name: 'UOM',
    },
    {
      id: 8,
      name: 'Client',
    },
    {
      id: 9,
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
    4: <div>Under construction</div>,
    5: <div>Under construction</div>,
    6: <GstList />,
    7: <UomList />,
    8: <ClientList />,
    9: <HsnCodeList />
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
