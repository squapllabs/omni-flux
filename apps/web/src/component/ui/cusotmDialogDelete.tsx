import React, { FC, ReactNode } from 'react';
import {
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Typography,
  IconButton,
} from '@mui/material';
import CustomButton from './customButton';
import { Button } from '@mui/material';
import CloseIcon from '@mui/icons-material/Close';
interface DialogProps {
  open: boolean;
  title: string;
  content: ReactNode;
  // onClose: () => void;
  handleClose: () => void;
  handleConfirm: () => void;
}

const CustomDialogBox: FC<DialogProps> = ({
  open,
  title,
  content,
  handleClose,
  handleConfirm,
}) => {
  return (
    <Dialog open={open} onClose={handleClose}>
      <DialogTitle>
        {' '}
        <div style={{ display: 'flex' }}>
          <Typography variant="h6" component="div" style={{ flexGrow: '5' }}>
            {title}
          </Typography>
          <IconButton color="default" onClick={handleClose}>
            <CloseIcon />
          </IconButton>
        </div>
      </DialogTitle>
      <DialogContent dividers>{content}</DialogContent>
      <DialogActions></DialogActions>
    </Dialog>
  );
};

export default CustomDialogBox;
