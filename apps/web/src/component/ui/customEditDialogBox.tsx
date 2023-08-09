import React from 'react';

interface DialogBoxProps {
  open: boolean;
  content: React.ReactNode;
}

const DialogBox: React.FC<DialogBoxProps> = ({
  open,
  content,

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
    padding: '2px',
    borderRadius: '4px',
    boxShadow: '0 0 10px rgba(0, 0, 0, 0.2)',
    maxWidth: '600px',
    display: 'flex',
    flexDirection: 'column',
  };

  return (
    <div style={dialogStyle}>
      <div style={boxStyle}>
        <div>{content}</div>
      </div>
    </div>
  );
};

export default DialogBox;
