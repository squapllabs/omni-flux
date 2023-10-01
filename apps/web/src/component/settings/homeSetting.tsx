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

const Settings = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const roleName =
    encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
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
      { label: 'Master Data', value: 'M' },
      { label: 'Users', value: 'U' },
      { label: 'Vendors', value: 'VL' },
      { label: 'Labours', value: 'LB' },
      { label: 'GST', value: 'GST' },
      { label: 'UOM', value: 'UOM' },
      { label: 'Client', value: 'CL' },
      { label: 'HsnCode', value: 'HC' },
      { label: 'Items', value: 'IL' }
    );
  }

  const [buttonLabels, setButtonLabels] = useState(menuItems);

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  return (
    <div>
      <div className={Styles.container}>
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
      </div>
    </div>
  );
};
export default Settings;
