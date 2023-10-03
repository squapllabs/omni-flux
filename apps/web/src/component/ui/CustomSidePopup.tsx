import React from 'react';
import Styles from '../../styles/customPopup.module.scss';
import CloseIcon from '../menu/icons/closeIcon';
interface DialogBoxProps {
  open: boolean;
  content: React.ReactNode;
  title: String;
  handleClose: () => void;
}

const CustomSidePopup: React.FC<DialogBoxProps> = ({
  open,
  content,
  title,
  handleClose,
}) => {
  if (!open) return null;
  const dialogStyle: React.CSSProperties = {
    position: 'fixed',
    top: '0',
    right: '0', // Set to right to make it a right-side popup
    width: '100%', // Set to 50% to cover half the width
    height: '100%',
    backgroundColor: 'rgba(0, 0, 0, 0.6)',
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
  };

  const boxStyle: React.CSSProperties = {
    backgroundColor: '#fff',
    padding: '2px',
    borderRadius: '4px',
    boxShadow: '0 0 10px rgba(0, 0, 0, 0.2)',
    width: '100%', // Set to 100% to cover the full width inside the popup
    height: '100%', // Set to 100% to cover the full height inside the popup
    display: 'flex',
    flexDirection: 'column',
  };

  return (
    <div className={Styles.popupContainer}>
      <div className={Styles.leftdialogStyle}></div>
      <div className={Styles.dialogStyle}>
        <div className={Styles.boxStyle}>
          <div className={Styles.mainContent}>
            <div className={Styles.popupHeader}>
              <h4>{title}</h4>
              <button className={Styles.closeButton}>
                <CloseIcon onClick={handleClose} />
              </button>
            </div>
          </div>
          <div className={Styles.selected}></div>
          <div className={Styles.main_content}>{content}</div>
        </div>
      </div>
    </div>
  );
};

export default CustomSidePopup;
