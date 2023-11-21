import React, { useEffect, useState } from 'react';
import CheckIcon from '../menu/icons/CheckIcon';
import WarningIcon from '../menu/icons/warningIcon';
interface CustomSnackbarProps {
  open: boolean;
  message: string;
  onClose: () => void;
  autoHideDuration?: number;
  type: 'success' | 'error' | 'warning';
}

const CustomSnackbar: React.FC<CustomSnackbarProps> = ({
  open,
  message,
  onClose,
  autoHideDuration = 3000,
  type = 'success',
}) => {
  const [isVisible, setIsVisible] = useState(open);

  useEffect(() => {
    setIsVisible(open);
  }, [open]);

  useEffect(() => {
    if (isVisible && autoHideDuration) {
      const timeoutId = setTimeout(() => {
        setIsVisible(false);
        onClose();
      }, autoHideDuration);

      return () => clearTimeout(timeoutId);
    }
  }, [isVisible, autoHideDuration, onClose]);

  if (!isVisible) return null;

  let icon;
  let fontColor;

  switch (type) {
    case 'success':
      icon = <CheckIcon color='#418944'/>;
      fontColor = '#1e4620';
      break;
    case 'error':
      icon = <WarningIcon color='#d94b4b'/>;
      fontColor = '#5f2120';
      break;
    case 'warning':
      icon = <WarningIcon color='#f08025'/>;
      fontColor = '#683e02';
      break;
    default:
      icon = null;
  }

  return (
    <div
      style={{
        position: 'fixed',
        bottom: '16px',
        left: '50%',
        transform: 'translateX(-50%)',
        backgroundColor: type === 'success' ? '#edf7ed' : type === 'error' ? '#fdeded' : '#fff4e5',
        color: fontColor,
        padding: '8px 16px',
        borderRadius: '4px',
        boxShadow: '0px 4px 8px rgba(0, 0, 0, 0.1)',
        display: 'flex',
        alignItems: 'center',
      }}
    >
      {icon && <div style={{ marginRight: '8px' }}>{icon}</div>}
      {message}
    </div>
  );
};

export default CustomSnackbar;
