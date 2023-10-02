import React from 'react';

interface DialogBoxProps {
  open: boolean;
  content: React.ReactNode;
}

const CustomRightPopup: React.FC<DialogBoxProps> = ({ open, content }) => {
  if (!open) return null;

  const dialogStyle: React.CSSProperties = {
    position: 'fixed',
    top: '0',
    right: '0',  // Set to right to make it a right-side popup
    width: '50%',  // Set to 50% to cover half the width
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
    width: '100%',  // Set to 100% to cover the full width inside the popup
    height: '100%', // Set to 100% to cover the full height inside the popup
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

export default CustomRightPopup;
