import React from 'react';
import {
  FormControl,
  FormLabel,
  RadioGroup,
  FormControlLabel,
  Radio,
} from '@mui/material';

interface Option {
  value: string;
  label: string;
  name: string;
}

interface RadioGroupButtonProps {
  label: string;
  options: Option[];
  value: string;
  name: string;
  onChange: (event: React.ChangeEvent<HTMLInputElement>) => void;
}

const CustomRadioButtonGroup: React.FC<RadioGroupButtonProps> = ({
  label,
  options,
  value,
  name,
  onChange,
}) => {
  return (
    <FormControl component="fieldset">
      <FormLabel component="legend">{label}</FormLabel>
      <RadioGroup
        aria-label={label}
        value={value}
        onChange={onChange}
        name={name}
      >
        {options.map((option) => (
          <FormControlLabel
            key={option.value}
            value={option.value}
            control={<Radio />}
            label={option.label}
          />
        ))}
      </RadioGroup>
    </FormControl>
  );
};

export default CustomRadioButtonGroup;
