import React, { useState } from 'react';
import Styles from '../../styles/category.module.scss';
import SideNav from '../ui/sideNav';
import CategoryList from './categoryList';
import SubCategoryList from '../subCategory/subCategoryList';
import SubSubCategoryList from '../subSubCategory/subSubList';
interface MainContentProps {
  id: number;
}
const Category = () => {
  const [selectedItem, setSelectedItem] = useState<number>(1);
  const menuItems = [
    {
      id: 1,
      name: 'Catgeory',
    },
    {
      id: 2,
      name: 'Sub Catgeory',
    },
    {
      id: 3,
      name: 'Sub Sub Catgeory',
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
  };
  console.log(
    'mainContentComponents[selectedItem]',
    mainContentComponents[selectedItem]
  );

  return (
    <>
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
    </>
  );
};

export default Category;
