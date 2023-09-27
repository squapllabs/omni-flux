import React, { useState } from 'react';
import Styles from '../../styles/homeSettings.module.scss';
import CustomGroupButton from '../ui/CustomGroupButton';
import Category from '../category/category';
import UserList from '../users/userList';
import MasterData from '../masterData/masterData';
import LeadList from '../leadEnquires/leadList';
import VendorList from '../vendor/vendorList';
import LabourList from '../labour/labourList';
import PurchaseList from '../purchaseApproval/purchaseList';
import PlanEngApproval from '../indentApproval/indentList';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import SideNav from '../ui/SubmenuNav';
import PurchaseOrderList from '../purchaseOrder/purchaseOrder';
import FinanceInvoiceView from '../finance/invoiceView';
import ExpenseApprove from '../expanses/siteExpenseApprove';

const Settings = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const roleName = encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
  const [selectedItem, setSelectedItem] = useState<number>(1);
  const [activeButton, setActiveButton] = useState<string | null>(()=>{
    let type=null
    if(roleName=== 'ADMIN')
    type='C'
    else if(roleName=== 'PLANNING ENGINEER' || roleName=== 'PROJECT MANAGER')
    type='IA'
    else if(roleName=== 'PURCHASE MANAGER')
    type='PL'
    else if(roleName=== 'FINANCE MANAGER')
    type='FMV'
    return type;
  });

  const menuItems = [];

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
  if (roleName === 'ADMIN') {
    menuItems.push(
      { label: 'Category', value: 'C' },
      { label: 'Master Data', value: 'M' },
      { label: 'Users', value: 'U' },
      // { label: 'Lead List', value: 'LL' },
      { label: 'Vendors', value: 'VL' },
      { label: 'Labours', value: 'LB' },
      { label: 'Indent Approval', value: 'IA' },
      { label: 'Purchase Request', value: 'PL' },
      { label: 'Purchase Order', value: 'PO' },
      { label: 'Expense Approve', value: 'EA' },
      { label: 'Invoice', value: 'FMV' }
    );

  }
  if (roleName === 'PLANNING ENGINEER') {
    menuItems.push({ label: 'Indent Approval', value: 'IA' });

  }
  if (roleName === 'PURCHASE MANAGER') {
    menuItems.push(
      { label: 'Purchase Request', value: 'PL' },
      { label: 'Purchase Order', value: 'PO' },
      { label: 'Vendors', value: 'VL' },
    );

  }
  if (roleName === 'FINANCE MANAGER') {
    menuItems.push({ label: 'Invoice', value: 'FMV' });

  }

  if (roleName === 'PROJECT MANAGER') {
    menuItems.push(
    { label: 'Indent Approval', value: 'IA' },
    { label: 'Purchase Order', value: 'PO' },
    { label: 'Purchase Request', value: 'PL' },
    { label: 'Expense Approve', value: 'EA' },
    { label: 'Invoice', value: 'FMV' }
    );
  }
  
  const [buttonLabels, setButtonLabels] = useState(menuItems);

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  const handleMenuItemClick = (id: number) => {
    setSelectedItem(id);
  };
  return (
    <div>
      <div className={Styles.container}>
        {/* <div className={Styles.heading}>Settings</div> */}
        <div className={Styles.button}>
          <CustomGroupButton
            labels={buttonLabels}
            onClick={handleGroupButtonClick}
            activeButton={activeButton}
          />
          {roleName === 'ADMIN' && activeButton === 'C' && (
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
        {activeButton === 'C' && <Category selectedItem={selectedItem} />}
        {activeButton === 'M' && <MasterData />}
        {activeButton === 'U' && <UserList />}
        {activeButton === 'LL' && <LeadList />}
        {activeButton === 'VL' && <VendorList />}
        {activeButton === 'LB' && <LabourList />}
        {activeButton === 'IA' && <PlanEngApproval />}
        {activeButton === 'PL' && <PurchaseList />}
        {activeButton === 'PO' && <PurchaseOrderList />}
        {activeButton === 'FMV' && <FinanceInvoiceView />}
        {activeButton === 'EA' && <ExpenseApprove />}
      </div>
    </div>
  );
};
export default Settings;
