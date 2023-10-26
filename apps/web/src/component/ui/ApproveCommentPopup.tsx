import React, { useState } from 'react';
import Button from '../ui/Button';
import CancelIcon from '../menu/icons/closeIcon';
import TextArea from './CustomTextArea';

interface DialogBoxProps {
  title: string;
  open: boolean;
  handleClose: () => void;
  contentLine1: string;
  contentLine2: string;
  onReject: (comments: string) => void;
}

const ApproveCommentDialogBox: React.FC<DialogBoxProps> & {
  onReject: (comments: string) => void;
} = ({ title, open, handleClose, contentLine1, contentLine2, onReject }) => {
  const [comments, setComments] = useState('');

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
    maxWidth: '400px',
    display: 'flex',
    flexDirection: 'column',
  };

  const titleStyle: React.CSSProperties = {
    padding: '10px 10px 1px 10px',
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
          </div>
          <div style={iconContentStyle}>
            <CancelIcon onClick={handleClose} />
          </div>
        </div>
        <div style={dividerStyle}></div>
        <div style={bodyContent}>
          <p style={contentStyle}>{contentLine1}</p>
          <p style={contentStyle}>{contentLine2}</p>
          <div style={textStyle}>
            <TextArea
              name="comments"
              label="Comments"
              placeholder="Enter comments"
              value={comments}
              rows={3}
              onChange={(e) => setComments(e.target.value)}
            />
          </div>
        </div>
        <div style={dividerStyle}></div>
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
            color='primary'
            shape="rectangle"
            justify="center"
            size="small"
            onClick={() => {
              onReject(comments);
            }}
          >
            Recall
          </Button>
        </div>
      </div>
    </div>
  );
};

export default ApproveCommentDialogBox;
