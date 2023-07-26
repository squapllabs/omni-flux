import React from 'react';
import { FieldAttributes } from 'formik';
import { FormControl, InputLabel, MenuItem, Select } from '@mui/material';

interface Option {
  value: string;
  label: string;
}

interface SelectFieldProps extends FieldAttributes<any> {
  label: string;
  options: Option[];
}

const CustomSelect: React.FC<SelectFieldProps> = ({
  label,
  options,
  ...props
}) => {
  return (
    <FormControl>
      <InputLabel id="select-label">{label}</InputLabel>
      <Select labelId="select-label" id={props.name} {...props}>
        {options.map((option) => (
          <MenuItem key={option.value} value={option.value}>
            {option.label}
          </MenuItem>
        ))}
      </Select>
    </FormControl>
  );
};

export default CustomSelect;
