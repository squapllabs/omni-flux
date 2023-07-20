import React ,{ useState } from 'react';
import Styles from '../../styles/homeSettings.module.scss';
import User from '../users/userList';
import Gst from '../gst/gstList';
import Client from '../client/clientList';
import Uom from '../uom/uomList';
import HsnCode from '../hsnCode/hsnCodeList';
import Category from '../category/categoryList';

const HomeSetting = () => {
  const [activeComponent, setActiveComponent] = useState<string | null>('User');

  const handleButtonClick = (componentName: string | null) => {
    setActiveComponent(componentName);
  };

  const renderComponent = () => {
    switch (activeComponent) {
      case 'User':
        return <User />;
        case 'Gst':
          return <Gst />;
        case 'Client':
          return <Client />;
      case 'Uom':
        return <Uom />;
        case 'HsnCode':
          return <HsnCode />;
        case 'Category':
          return <Category />;
      default:
        return <User />;
    }
  };
  return (
    <div className={Styles.container}>
      <div className={Styles.containerOne}>
        <div className={Styles.demo}>
          <h2>Settings</h2>
          <div className={Styles.buttongroup}>
            <button className={activeComponent === 'User' ? Styles.activeButton : ''} onClick={() => handleButtonClick('User')}>User</button>
            <button className={activeComponent === 'Gst' ? Styles.activeButton : ''}  onClick={() => handleButtonClick('Gst')}>Gst</button>
            <button className={activeComponent === 'Client' ? Styles.activeButton : ''}  onClick={() => handleButtonClick('Client')}>Client</button>
            <button className={activeComponent === 'Uom' ? Styles.activeButton : ''}  onClick={() => handleButtonClick('Uom')}>Uom</button>
            <button className={activeComponent === 'HsnCode' ? Styles.activeButton : ''}  onClick={() => handleButtonClick('HsnCode')}>HsnCode</button>
            <button className={activeComponent === 'Category' ? Styles.activeButton : ''}  onClick={() => handleButtonClick('Category')}>Category</button>
          </div>
        </div>
      </div>
      {renderComponent()}
    </div>
  );
};

export default HomeSetting;
