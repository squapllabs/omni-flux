import React, { useState } from 'react';
import Styles from '../../styles/category.module.scss';
import CategoryList from './categoryList';
import SubCategoryList from '../subCategory/subCategoryList';
import SubSubCategoryList from '../subSubCategory/subSubList';
import GstList from '../gst/gstList';
import UomList from '../uom/uomList';
import ClientList from '../client/clientList';
import HsnCodeList from '../hsnCode/hsnCodeList';

interface CategoryProps {
  selectedItem: number;
}

const Category: React.FC<CategoryProps> = ({ selectedItem }) => {
  const mainContentComponents: { [key: number]: JSX.Element } = {
    1: <CategoryList />,
    2: <SubCategoryList />,
    3: <SubSubCategoryList />,
    4: <GstList />,
    5: <UomList />,
    6: <ClientList />,
    7: <HsnCodeList />,
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
