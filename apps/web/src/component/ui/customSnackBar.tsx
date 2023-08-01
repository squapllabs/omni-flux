import React, { useEffect, useState } from 'react';

interface CustomSnackbarProps {
  open: boolean;
  message: string;
  onClose: () => void;
  autoHideDuration?: number;
  type: 'success' | 'error';
}

const CustomSnackbar: React.FC<CustomSnackbarProps> = ({
  open,
  message,
  onClose,
  autoHideDuration = 3000,
  type = 'success' 
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

  const backgroundColor = (type === 'success' ? '#916DB3' : 'red');

  return (
    <div
      style={{
        position: 'fixed',
        bottom: '16px',
        left: '50%',
        transform: 'translateX(-50%)',
        backgroundColor: backgroundColor,
        color: 'white',
        padding: '8px 16px',
        borderRadius: '4px',
        boxShadow: '0px 4px 8px rgba(0, 0, 0, 0.1)',
      }}
    >
      {message}
    </div>
  );
};

export default CustomSnackbar;
