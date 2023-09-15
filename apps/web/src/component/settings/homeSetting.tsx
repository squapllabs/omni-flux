import React, { useState } from 'react';
import Styles from '../../styles/homeSettings.module.scss';
import CustomGroupButton from '../ui/CustomGroupButton';
import Category from '../category/category';
import UserList from '../users/userList';
import MasterData from '../masterData/masterData';
import ProjectList from '../project/projectList';
import LeadList from '../leadEnquires/leadList';
import Submenunav from '../ui/SubmenuNav';
import CategoryList from '../category/categoryList';
import SubCategoryList from '../subCategory/subCategoryList';
import SubSubCategoryList from '../subSubCategory/subSubList';
import GstList from '../gst/gstList';
import HsnCodeList from '../hsnCode/hsnCodeList';
import UomList from '../uom/uomList';
import ClientList from '../client/clientList';
import RoleFeature from '../roleFeature/userList';
const Settings = () => {
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Master Data', value: 'M' },
    { label: 'Category', value: 'C' },
    { label: 'User List', value: 'U' },
    { label: 'Project List', value: 'PL' },
    { label: 'Lead List', value: 'LL' },
    { label: 'Role Feature', value: 'RL' },
  ]);

  const [selectedItem, setSelectedItem] = useState<number>(1);
  console.log(selectedItem);
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
    7: <HsnCodeList />,
  };

  const [activeButton, setActiveButton] = useState<string | null>('M');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  return (
    <div>
      <div className={Styles.container}>
        <div className={Styles.heading}>Settings</div>
        <div className={Styles.button}>
          <CustomGroupButton
            labels={buttonLabels}
            onClick={handleGroupButtonClick}
            activeButton={activeButton}
          />
          {activeButton == 'C' && (
            <Submenunav
              menuItems={menuItems}
              selectedItem={selectedItem}
              handleMenuItemClick={handleMenuItemClick}
            />
          )}
        </div>
      </div>
      <div className={Styles.dividerLine}></div>
      <div>
        {activeButton === 'C' && <Category selectedItem={selectedItem} />}
        {activeButton === 'M' && <MasterData />}
        {activeButton === 'U' && <UserList />}
        {activeButton === 'PL' && <ProjectList />}
        {activeButton === 'LL' && <LeadList />}
        {activeButton === 'RL' && <RoleFeature />}
      </div>
    </div>
  );
};
export default Settings;
