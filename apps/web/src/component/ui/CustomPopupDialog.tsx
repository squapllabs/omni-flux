import React from 'react';
import Styles from '../../styles/customPopup.module.scss';

interface CustomPopupProps {
  className:string;
    children: React.ReactNode; 
  }

const CustomPopup: React.FC<CustomPopupProps> = ({ children ,className}) => {
  return (
    <div className={Styles.popupOverlay}>
      <div>
        <div className={Styles.popupBody}>{children}</div>
      </div>
    </div>
  );
};

export default CustomPopup;
