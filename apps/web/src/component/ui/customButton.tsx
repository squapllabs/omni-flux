import React, { ReactNode } from 'react';
import Button, { ButtonProps } from '@mui/material/Button';

interface MyButtonProps extends ButtonProps {
  label: string;
  startIcon?: ReactNode;
  endIcon?: ReactNode;
}

const CustomButton: React.FC<MyButtonProps> = ({
  label,
  onClick,
  color,
  startIcon,
  endIcon,
  ...rest
}) => {
  return (
    <Button
      variant="contained"
      color={color}
      startIcon={startIcon}
      endIcon={endIcon}
      onClick={onClick}
      {...rest}
    >
      {label}
    </Button>
  );
};

export default CustomButton;
