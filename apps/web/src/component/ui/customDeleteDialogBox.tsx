import React from 'react';
import DeleteIcon from '../menu/icons/deleteIcon';
import Button from '../ui/Button';
import CancelIcon from '../menu/icons/closeIcon';

interface DialogBoxProps {
  title: string;
  open: boolean;
  handleClose: () => void;
  handleConfirm: () => void;
  contentLine1: string;
  contentLine2: string;
}

const DialogBox: React.FC<DialogBoxProps> = ({
  title,
  open,
  handleClose,
  handleConfirm,
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
    padding: '10px 10px 1px 10px',
  };

  const contentStyle: React.CSSProperties = {
    padding: '8px',
    fontSize: 'smaller',
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

  const conformButtonStyle: React.CSSProperties = {
    ...buttonStyle,
    backgroundColor: '#D92D20',
    color: 'white',
    borderRadius: '8px',
  };

  const mainContentStyle: React.CSSProperties = {
    display: 'flex', 
    flexDirection: 'row'
  };

  const iconContentStyle: React.CSSProperties = {
    padding: '15px'
  };

  return (
    <div style={dialogStyle}>
      <div style={boxStyle}>
        <div style={mainContentStyle}>
          <div style={iconContentStyle}>
              <DeleteIcon color="red" />
          </div>
          <div>
            <h4 style={titleStyle}>{title}</h4>
            <p style={contentStyle}>{contentLine1}</p>
            <p style={contentStyle}>{contentLine2}</p>
          </div>
          <div style={iconContentStyle}>
            <CancelIcon onClick={handleClose} />
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
          >
            Cancel
          </Button>
          <Button
            style={conformButtonStyle}
            shape="rectangle"
            justify="center"
            size="small"
            onClick={handleConfirm}
          >
            Delete
          </Button>
        </div>
      </div>
    </div>
  );
};

export default DialogBox;
