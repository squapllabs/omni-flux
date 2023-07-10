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
import CloseIcon from '@mui/icons-material/Close';
interface DialogProps {
  open: boolean;
  title: string;
  content: ReactNode;
  onClose: () => void;
}

const CustomDialog: FC<DialogProps> = ({ open, title, content, onClose }) => {
  return (
    <Dialog open={open} onClose={onClose}>
      <DialogTitle>
        {' '}
        <div style={{ display: 'flex' }}>
          <Typography variant="h6" component="div" style={{ flexGrow: '5' }}>
            {title}
          </Typography>
          <IconButton color="default" onClick={onClose}>
            <CloseIcon />
          </IconButton>
        </div>
      </DialogTitle>
      <DialogContent dividers>{content}</DialogContent>
      <DialogActions></DialogActions>
    </Dialog>
  );
};

export default CustomDialog;
