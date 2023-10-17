import React, { useState } from 'react';
import Styles from '../../styles/homeSettings.module.scss';
import CustomGroupButton from '../ui/CustomGroupButton';
import UserList from '../users/userList';
import MasterData from '../masterData/masterData';
import VendorList from '../vendor/vendorList';
import LabourList from '../labour/labourList';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import GstList from '../gst/gstList';
import UomList from '../uom/uomList';
import ClientList from '../client/clientList';
import HsnCodeList from '../hsnCode/hsnCodeList';
import ItemList from '../products/productPage';
import ProjectSubheader from '../project/projectSubheader';
import CustomLoader from '../ui/customLoader';
import SideNav from '../ui/sideNav';
import MachineryList from '../machinery/machineryList';
import ContractorList from '../contractor/contractorList';

const Settings = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const roleName =
    encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
  const [loader, setLoader] = useState(false);
  const [activeButton, setActiveButton] = useState<string | null>(() => {
    let type = null;
    if (roleName === 'ADMIN') type = 'M';
    else if (roleName === 'PLANNING ENGINEER' || roleName === 'PROJECT MANAGER')
      type = 'IA';
    else if (roleName === 'PURCHASE MANAGER') type = 'PL';
    else if (roleName === 'FINANCE MANAGER') type = 'FMV';
    return type;
  });

  const menuItems = [];
  if (roleName === 'ADMIN') {
    menuItems.push(
      { name: 'Master Data', id: 1 },
      { name: 'Users', id: 2 },
      { name: 'Vendors', id: 3 },
      { name: 'Labours', id: 4 },
      { name: 'GST', id: 5 },
      { name: 'UOM', id: 6 },
      { name: 'Client', id: 7 },
      { name: 'HSN Code', id: 8 },
      { name: 'Items', id: 9 },
      { name: 'Machineries', id: 10 },
      { name: 'Contractors', id: 11 },
    );
  }

  const [buttonLabels, setButtonLabels] = useState(menuItems);
  const [selectedItem, setSelectedItem] = useState<number>(1);

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  const mainContentComponents: { [key: number]: JSX.Element } = {
    1: (
      <MasterData
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    2: (
      <UserList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    3: (
      <VendorList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    4: (
      <LabourList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    5: (
      <GstList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    6: (
      <UomList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    7: (
      <ClientList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    8: (
      <HsnCodeList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    9: (
      <ItemList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    10: (
      <MachineryList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    11: (
      <ContractorList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
  };

  const handleMenuItemClick = (id: number) => {
    setSelectedItem(id);
  };

  return (
    <CustomLoader loading={loader} size={48}>
      <div className={Styles.Container}>
        <div>
          <ProjectSubheader
            title="Settings"
            navigation="/home"
            description="Manage your master datas across your application"
          />
        </div>
        <div className={Styles.selected}></div>
        <div className={Styles.mainContainer}>
          <div className={Styles.sidnav}>
            <SideNav
              menuItems={menuItems}
              selectedItem={selectedItem}
              handleMenuItemClick={handleMenuItemClick}
            />
          </div>
          <div className={Styles.mainbar}>
            {mainContentComponents[selectedItem]}
          </div>
        </div>
        {/* <div className={Styles.container}>
        <div className={Styles.button}>
          <CustomGroupButton
            labels={buttonLabels}
            onClick={handleGroupButtonClick}
            activeButton={activeButton}
          />
        </div>
      </div>
      <div className={Styles.dividerLine}></div>
      <div>
        {activeButton === 'M' && <MasterData />}
        {activeButton === 'U' && <UserList />}
        {activeButton === 'VL' && <VendorList />}
        {activeButton === 'LB' && <LabourList />}
        {activeButton === 'GST' && <GstList />}
        {activeButton === 'UOM' && <UomList />}
        {activeButton === 'CL' && <ClientList />}
        {activeButton === 'HC' && <HsnCodeList />}
        {activeButton === 'IL' && <ItemList />}
      </div> */}
      </div >
    </CustomLoader >
  );
};
export default Settings;
