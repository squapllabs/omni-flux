import React from 'react';
// import DeleteIcon from '../menu/icons/deleteIcon';
import Button from '../ui/Button';
import CancelIcon from '../menu/icons/closeIcon';
import FlagIcon from '../menu/icons/flagIcon';
import Styles from '../../styles/customSwitch.module.scss';

interface DialogBoxProps {
  title: string;
  open: boolean;
  handleClose: () => void;
  contentLine1: string;
  contentLine2: string;
}

const CustomDialogBox: React.FC<DialogBoxProps> = ({
  title,
  open,
  handleClose,
  contentLine1,
  contentLine2,
}) => {
  if (!open) return null;

  const dialogStyle: React.CSSProperties = {
    position: 'fixed',
    top: '0',
    left: '0',
    width: '100%',
    height: '100%',
    backgroundColor: 'rgba(0, 0, 0, 0.6)',
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
  };

  const boxStyle: React.CSSProperties = {
    backgroundColor: '#fff',
    padding: '20px',
    borderRadius: '4px',
    boxShadow: '0 0 10px rgba(0, 0, 0, 0.2)',
    maxWidth: '400px',
    display: 'flex',
    flexDirection: 'column',
  };

  const titleStyle: React.CSSProperties = {
    fontSize: '20px',
  };

  const contentStyle: React.CSSProperties = {
    paddingTop: '10px',
    paddingLeft: '10px',
    paddingRight: '10px',
    fontSize: '15px',
  };

  const buttonStyle: React.CSSProperties = {
    margin: '10px',
    padding: '6px 12px',
    border: 'none',
    borderRadius: '4px',
    cursor: 'pointer',
  };

  const cancelButtonStyle: React.CSSProperties = {
    ...buttonStyle,
    borderRadius: '8px',
  };

  // const conformButtonStyle: React.CSSProperties = {
  //   ...buttonStyle,
  //   backgroundColor: '#D92D20',
  //   color: 'white',
  //   borderRadius: '8px',
  // };

  const mainContentStyle: React.CSSProperties = {
    display: 'flex',
    flexDirection: 'row',
  };
  const headerContentStyle: React.CSSProperties = {
    display: 'flex',
    flexDirection: 'row',
    justifyContent: 'space-between',
    paddingBottom: '10px',
  };

  const iconContentStyle: React.CSSProperties = {
    display: 'flex',
    gap: '20px',
    alignItems: 'center',
    textAlign: 'center',
  };
  const cancelContentStyle: React.CSSProperties = {
    paddingRight: '20px',
    padding: '5px',
  };

  return (
    <div style={dialogStyle}>
      <div style={boxStyle}>
        <div className={Styles.box}>
          <div style={headerContentStyle}>
            <div style={iconContentStyle}>
              <FlagIcon color="red" />
              <h4 style={titleStyle}>{title}</h4>
            </div>
            <div style={cancelContentStyle}>
              <CancelIcon onClick={handleClose} />
            </div>
          </div>
        </div>
        <div className={Styles.box}>
          <div style={mainContentStyle}>
            <div>
              <p style={contentStyle}>{contentLine1}</p>
              <p style={contentStyle}>{contentLine2}</p>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              flexDirection: 'row',
              justifyContent: 'flex-end',
            }}
          >
            <Button
              style={cancelButtonStyle}
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleClose}
              color="primary"
            >
              OK
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default CustomDialogBox;
