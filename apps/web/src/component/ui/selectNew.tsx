import React, { FC } from 'react';
import styled from 'styled-components';

interface SelectProps {
  onChange: (event: React.ChangeEvent<HTMLSelectElement>) => void;
  value: string;
  defaultLabel: string;
  width?: string;
  children: React.ReactNode;
  label?: string;
  name?: string;
  error?: string;
}

interface StyledSelectProps {
  value: string;
  width?: string;
  error?: boolean;
}
const SelectContainer = styled.div<{ width?: string }>`
  display: flex;
  flex-direction: column;
  position: relative;
  width: ${(props) => props.width || '200px'};
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
  border: 1px solid ${(props) => (props.error ? 'red' : '#ccc')};
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

const StyledLabel = styled.label`
  margin-bottom: 4px;
  font-size: 0.75rem;
  color: #333c44;
  font-weight: 400;
`;

const ErrorMessageWrapper = styled.div`
  min-height: 20px; // Change to the height of your error message
`;

const InputError = styled.span`
  color: red;
  margin-top: 2px;
  font-size: 0.75rem;
`;
const Select: FC<SelectProps> = ({
  onChange,
  label,
  value,
  defaultLabel,
  width,
  children,
  name,
  error,
}) => {
  return (
    <div>
      {label && <StyledLabel>{label}</StyledLabel>}
      <SelectContainer width={width}>
        <StyledSelect
          value={value}
          name={name}
          onChange={onChange}
          error={!!error}
        >
          <option value="">{defaultLabel}</option>
          {children}
        </StyledSelect>
        <DropdownArrow />
      </SelectContainer>
      <ErrorMessageWrapper>
        {error && <InputError>{error}</InputError>}
      </ErrorMessageWrapper>
    </div>
  );
};

export default Select;
