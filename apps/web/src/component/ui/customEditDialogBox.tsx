import React from 'react';
// import CancelIcon from '../menu/icons/closeIcon';
import Button from '../ui/Button';
import FlagIcon from '../menu/icons/flagIcon';

interface DialogBoxProps {
  title: string;
  open: boolean;
  subTitle: string;
  content: React.ReactNode;
  // handleClose: () => void;
  // handleSubmit: () => void;
}

const DialogBox: React.FC<DialogBoxProps> = ({
  // title,
  open,
  // subTitle,
  content,
  // handleClose,
  // handleSubmit,
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

  // const titleStyle: React.CSSProperties = {
  //   padding: '10px 10px 1px 20px',
  // };

  // const subTitleStyle: React.CSSProperties = {
  //   fontSize: '12px',
  //   padding: '0px 0px 20px 20px',
  // };

  // const mainContentStyle: React.CSSProperties = {
  //   display: 'flex',
  //   justifyContent: 'flex-end',
  //   padding: '15px',
  // };

  // const dividerStyle: React.CSSProperties = {
  //   borderBottom: '1px solid #ccc',
  //   padding: '5px 1px 5px 1px',
  //   // backgroundColor:'lightgreen'
  // };
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
    // backgroundColor: '#D92D20',
    color: 'white',
    borderRadius: '8px',
  };

  return (
    <div style={dialogStyle}>
      <div style={boxStyle}>
        {/* <div style={{ display: 'flex', flexDirection: 'row',justifyContent:'space-between'}}>
          <div>
            <h4 style={titleStyle}>{title}</h4>
            <span style={subTitleStyle}>{subTitle}</span>
          </div>
          <div style={mainContentStyle}>
            <CancelIcon onClick={handleClose} />
          </div>
        </div> */}
        {/* <div style={dividerStyle}></div> */}
        <div>{content}</div>
        {/* <div
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
            onClick={handleSubmit}
          >
            Delete
          </Button>
        </div> */}
      </div>
    </div>
  );
};

export default DialogBox;
