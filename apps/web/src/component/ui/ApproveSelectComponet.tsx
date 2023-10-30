import React, { useState } from 'react';
import Button from '../ui/Button';
import CancelIcon from '../menu/icons/closeIcon';
import Select from './Select';

interface DialogBoxProps {
  title: string;
  open: boolean;
  handleClose: () => void;
  contentLine1: string;
  onApprove: (selectedValue: string) => void;
}

const ApproveSelectDialogBox: React.FC<DialogBoxProps> & {
  onReject: (comments: string) => void;
} = ({ title, open, handleClose, contentLine1, onApprove }) => {
  const [error, setError] = useState<string | null>(null);
  const [selectedValue, setSelectedValue] = useState('');

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

  const errorContent: React.CSSProperties = {
    color: 'red',
    fontSize: '2vh',
    paddingLeft: '5px',
  };

  const dividerStyle: React.CSSProperties = {
    borderTop: '2px solid #ccc',
  };

  const SampleOption: any = [
    { label: 'Head Office', value: 'Head Office' },
    { label: 'Local Purchase', value: 'Local Purchase' },
  ];

  const handleApprove = () => {
    if (selectedValue) {
      onApprove(selectedValue);
    } else {
      setError('Please select an option');
    }
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
          <div style={textStyle}>
            <Select
              options={SampleOption}
              onChange={(event) => {
                setSelectedValue(event.target.value);
                setError(null);
              }}
              value={selectedValue}
              defaultLabel="Select purchase type"
            />
          </div>
          {error && <p style={errorContent}>{error}</p>}
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
            color="primary"
            shape="rectangle"
            justify="center"
            size="small"
            onClick={handleApprove}
          >
            Approve
          </Button>
        </div>
      </div>
    </div>
  );
};

export default ApproveSelectDialogBox;
