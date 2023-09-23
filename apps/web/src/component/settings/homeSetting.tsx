import React, { useState } from 'react';
import Styles from '../../styles/homeSettings.module.scss';
import CustomGroupButton from "../ui/CustomGroupButton";
import Category from "../category/category";
import UserList from "../users/userList";
import MasterData from "../masterData/masterData";
import LeadList from "../leadEnquires/leadList";
import VendorList from "../vendor/vendorList";
import LabourList from '../labour/labourList';
import PurchaseList from '../purchaseApproval/purchaseList';
import PlanEngApproval from '../indentApproval/indentList';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import SideNav from '../ui/SubmenuNav';
const Settings = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const roleName = encryptedData?.userData?.user_roles[0]?.role_data?.role_name

  const menuItems = [
    { label: 'Category', value: 'C' },
    { label: 'Master Data', value: 'M' },
    { label: 'User List', value: 'U' },
    { label: 'Lead List', value: 'LL' },
    { label: 'Vendor List', value: 'VL' },
    { label: 'Labour List', value: 'LB' },
  ];

 const menuItemsCategory = [
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
  if (roleName === 'Planning Engineer') {
    menuItems.push({ label: 'Indent Approval', value: 'IA' });
  }
  if (roleName === 'Purchase Manager') {
    menuItems.push({ label: 'Purchase List', value: 'PL' });
  }
 
  const [selectedItem, setSelectedItem] = useState<number>(1);
  const [buttonLabels, setButtonLabels] = useState(menuItems);
  const [activeButton, setActiveButton] = useState<string | null>('C');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
   const handleMenuItemClick = (id: number) => {
    setSelectedItem(id);
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
            <SideNav
              menuItemsCategory={menuItemsCategory}
              selectedItem={selectedItem}
              handleMenuItemClick={handleMenuItemClick}
            />
          )}
        </div>
      </div>
      <div className={Styles.dividerLine}></div>
      <div>
        {activeButton === 'C' && <Category selectedItem={selectedItem}  />}
        {activeButton === 'M' && <MasterData />}
        {activeButton === 'U' && <UserList />}
        {activeButton === 'LL' && <LeadList />}
        {activeButton === 'VL' && <VendorList />}
        {activeButton === 'LB' && <LabourList />}
        {activeButton === 'IA' && <PlanEngApproval />}
        {activeButton === 'PL' && <PurchaseList />}
      </div>
    </div>
  );
};
export default Settings;