import React, { useState } from 'react';
import Styles from '../../styles/category.module.scss';
import SideNav from '../ui/sideNav';
import CategoryList from './categoryList';
import SubCategoryList from '../subCategory/subCategoryList';
import SubSubCategoryList from '../subSubCategory/subSubList';
import zIndex from '@mui/material/styles/zIndex';
interface MainContentProps {
  id: number;
}
const Category = () => {
  const [selectedItem, setSelectedItem] = useState<number>(2);
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
