import React, { FC } from 'react';
import styled from 'styled-components';

interface Option {
  value: string;
  label: string;
}

interface SelectProps {
  options: Option[];
  onChange: (event: React.ChangeEvent<HTMLSelectElement>) => void;
  value: string;
  defaultLabel: string;
  width?: string;
}

interface StyledSelectProps {
  value: string;
  width?: string;
}
const SelectContainer = styled.div<{ width?: string }>`
  display: flex;
  flex-direction: column;
  position: relative;
  width: ${(props) => props.width || "200px"};
`;

const StyledSelect = styled.select<StyledSelectProps>`
  appearance: none; // this is to remove default browser dropdown icon
  width: 100%;
  height: 38px;
  background: #f4f5f6;
  color: gray;
  padding: 6px 12px;
  font-size: 14px;
  border: none;
  border-radius: 4px;
  outline: none;

  option {
    color: black;
    background: white;
    display: flex;
    white-space: pre;
    min-height: 20px;
    padding: 0px 2px 1px;
  }

  &:hover {
    cursor: pointer;
  }
`;


// This is the dropdown arrow styled component
const DropdownArrow = styled.div`
  position: absolute;
  right: 10px;
  top: 50%;
  transform: translateY(-50%);
  width: 0;
  height: 0;
  border-left: 6px solid transparent;
  border-right: 6px solid transparent;
  border-top: 6px solid gray;
`;

const Select: FC<SelectProps> = ({ options, onChange, value, defaultLabel, width }) => {
  return (
    <SelectContainer width={width}>
      <StyledSelect value={value} onChange={onChange}>
        <option value="">{defaultLabel}</option>
        {options.map((option) => (
          <option key={option.value} value={option.value}>
            {option.label}
          </option>
        ))}
      </StyledSelect>
      <DropdownArrow />
    </SelectContainer>
  );
};

export default Select;
