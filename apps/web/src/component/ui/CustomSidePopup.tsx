import React from 'react';
import Styles from '../../styles/customPopup.module.scss';
import CloseIcon from '../menu/icons/closeIcon';
interface DialogBoxProps {
  open: boolean;
  content: React.ReactNode;
  title: String;
  handleClose: () => void;
  width: String;
  description: String;
}

const CustomSidePopup: React.FC<DialogBoxProps> = ({
  open,
  content,
  title,
  description,
  handleClose,
  width = '50%',
}) => {
  if (!open) return null;
  const styles = {
    width: width,
  };
  return (
    <div className={Styles.popupContainer}>
      <div className={Styles.leftdialogStyle}></div>
      <div className={Styles.dialogStyle} style={styles}>
        <div className={Styles.boxStyle}>
          <div className={Styles.mainContent}>
            <div className={Styles.popupHeader}>
              <div className={Styles.textContent_1}>
                <h4>{title}</h4>
                <span className={Styles.content}>{description}</span>
              </div>

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
