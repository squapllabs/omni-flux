import React from 'react';
import CancelIcon from '../menu/icons/closeIcon';

interface DialogBoxProps {
  title: string;
  open: boolean;
  handleClose: () => void;
  contentLine1: string;
  content: React.ReactNode;
}

const NewCustomPopupComponent: React.FC<DialogBoxProps> = ({
  title,
  open,
  handleClose,
  contentLine1,
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
    padding: '5px',
    borderRadius: '4px',
    boxShadow: '0 0 10px rgba(0, 0, 0, 0.2)',
    maxWidth: '1000px',
    width: '50%',
    // height: '50%',
    display: 'flex',
    flexDirection: 'column',
  };

  const titleStyle: React.CSSProperties = {
    padding: '10px 10px 1px 10px',
  };
  const descriptionStyle: React.CSSProperties = {
    padding: '10px 10px 1px 10px',
    fontWeight: '400',
    fontSize: '13px',
  };

  const contentStyle: React.CSSProperties = {
    padding: '10px',
    fontSize: 'smaller',
  };

  const textStyle: React.CSSProperties = {
    padding: '5px',
    fontSize: 'smaller',
  };

  const buttonStyle: React.CSSProperties = {
    margin: '15px',
    borderRadius: '4px',
    cursor: 'pointer',
  };

  const cancelButtonStyle: React.CSSProperties = {
    ...buttonStyle,
    borderRadius: '8px',
  };

  const conformButtonStyle: React.CSSProperties = {
    ...buttonStyle,
    // backgroundColor: '#D92D20',
    color: 'white',
    borderRadius: '8px',
  };

  const mainContentStyle: React.CSSProperties = {
    display: 'flex',
    flexDirection: 'row',
    justifyContent: 'space-between',
    padding: '5px',
  };

  const iconContentStyle: React.CSSProperties = {
    padding: '15px',
  };

  const bodyContent: React.CSSProperties = {
    padding: '10px',
    // maxHeight: '100px',
  };

  const errorContent: React.CSSProperties = {
    color: 'red',
    fontSize: '2vh',
    paddingLeft: '5px',
  };

  const dividerStyle: React.CSSProperties = {
    borderTop: '2px solid #ccc',
  };
  return (
    <div style={dialogStyle}>
      <div style={boxStyle}>
        <div style={mainContentStyle}>
          <div>
            <h4 style={titleStyle}>{title}</h4>
            <span style={descriptionStyle}>{contentLine1}</span>
          </div>
          <div style={iconContentStyle}>
            <CancelIcon onClick={handleClose} />
          </div>
        </div>
        <div style={dividerStyle}></div>
        <div style={bodyContent}>{content}</div>
      </div>
    </div>
  );
};

export default NewCustomPopupComponent;
