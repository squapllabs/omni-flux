import React from 'react';
import Snackbar from '@mui/material/Snackbar';
import MuiAlert, { Color } from '@mui/material/Alert';

interface MySnackbarProps {
  open: boolean;
  message: string;
  onClose: () => void;
  severity?: Color;
  autoHideDuration?: number;
}

const MySnackbar: React.FC<MySnackbarProps> = ({
  open,
  message,
  onClose,
  severity,
  autoHideDuration,
}) => {
  return (
    <Snackbar
      open={open}
      autoHideDuration={autoHideDuration}
      onClose={onClose}
      anchorOrigin={{ vertical: 'top', horizontal: 'center' }}
    >
      <MuiAlert onClose={onClose} severity={severity} sx={{ width: '100%' }}>
        {message}
      </MuiAlert>
    </Snackbar>
  );
};

export default MySnackbar;
