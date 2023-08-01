import React from 'react';
import CancelIcon from '../menu/icons/closeIcon';
import FlagIcon from '../menu/icons/flagIcon';

interface DialogBoxProps {
  title: string;
  open: boolean;
  subTitle : string;
  content: string;
  handleClose: () => void;
  
}

const DialogBox: React.FC<DialogBoxProps> = ({
  title,
  open,
  subTitle,
  content,
  handleClose,
  
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
    padding: '10px 10px 1px 10px',
  };

  const subTitleStyle: React.CSSProperties = {
    fontSize:'12px',
    padding:'0px 0px 20px 12px'
  };

  const mainContentStyle: React.CSSProperties = {
    display: 'flex', 
    flexDirection: 'row',
    justifyContent:'space-between'
  };

  const iconContentStyle: React.CSSProperties = {
    padding: '15px'
  };
  return (
    <div style={dialogStyle}>
      <div style={boxStyle}>
        <div style={mainContentStyle}>
          <div style={iconContentStyle}>
            <FlagIcon />
          </div>
          <div style={iconContentStyle}>
            <CancelIcon onClick={handleClose} />
          </div>
        </div>
        <div>
          <h4 style={titleStyle}>{title}</h4>
          <span style={subTitleStyle}>{subTitle}</span>
          <div >{content}</div>
        </div>
      </div>
    </div>
  );
};

export default DialogBox;