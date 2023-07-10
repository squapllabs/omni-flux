import React from 'react';
import { TextField, TextFieldProps } from '@mui/material';

interface CustomTextFieldProps extends TextFieldProps {
  name: string;
  label: string;
  size?: 'small' | 'medium' | 'large';
  type?: string;
  variant?: 'standard' | 'filled' | 'outlined' | 'default';
  inputProps?: object;
}

const CustomTextField: React.FC<CustomTextFieldProps> = ({
  name,
  label,
  size = 'medium',
  type = 'text',
  variant = 'standard',
  inputProps,
  ...rest
}) => {
  return (
    <TextField
      name={name}
      label={label}
      size={size}
      type={type}
      variant={variant}
      fullWidth
      InputProps={inputProps}
      {...rest}
    />
  );
};

export default CustomTextField;
