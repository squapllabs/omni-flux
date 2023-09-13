import React from 'react';
import CancelIcon from '../menu/icons/closeIcon';
import FlagIcon from '../menu/icons/flagIcon';

interface DialogBoxProps {
  title: string;
  open: boolean;
  subTitle: string;
  content: React.ReactNode;
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
    padding: '10px',
    borderRadius: '4px',
    boxShadow: '0 0 10px rgba(0, 0, 0, 0.2)',
    maxWidth: '400px',
    display: 'flex',
    flexDirection: 'column',
  };

  const titleStyle: React.CSSProperties = {
    padding: '10px 10px 1px 20px',
  };

  const subTitleStyle: React.CSSProperties = {
    fontSize: '12px',
    padding: '0px 0px 20px 20px',
  };

  const mainContentStyle: React.CSSProperties = {
    display: 'flex',
    justifyContent: 'flex-end',
    padding: '15px',
  };

  const dividerStyle: React.CSSProperties = {
    borderBottom: '1px solid #ccc',
    padding: '5px 1px 5px 1px',
  };
  return (
    <div style={dialogStyle}>
      <div style={boxStyle}>
        <div style={{ display: 'flex', flexDirection: 'row',justifyContent:'space-between' }}>
          <div>
            <h4 style={titleStyle}>{title}</h4>
            <span style={subTitleStyle}>{subTitle}</span>
          </div>
          <div style={mainContentStyle}>
            <CancelIcon onClick={handleClose} />
          </div>
        </div>
        <div style={dividerStyle}></div>
        <div>{content}</div>
      </div>
    </div>
  );
};

export default DialogBox;
