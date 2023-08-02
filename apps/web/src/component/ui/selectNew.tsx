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
  error?: boolean;
}
interface InputWrapperProps {
  width?: string;
}
interface StyledSelectProps {
  value: string;
  width?: string;
  error?: boolean;
}

const InputWrapper = styled.div<InputWrapperProps>`
  display: flex;
  flex-direction: column;
  width: ${(props) => props.width || '100%'};
`;
const SelectContainer = styled.div<StyledSelectProps>`
  position: relative;
  display: flex;
  align-items: center;
  border: 1px solid ${(props) => (props.error ? 'red' : '#ccc')};
  border-radius: 4px;
  background-color: #f4f5f6;
  &:hover {
    border-color: #888;
  }
  &:focus-within {
    outline: 0;
    box-shadow: 0 0 0 2px #68717840;
  }
`;
const StyledSelect = styled.select<StyledSelectProps>`
  appearance: none; // this is to remove default browser dropdown icon
  width: 100%;
  height: 38px;
  background: #f4f5f6;
  color: ${(props) => (props.value === '' ? 'gray' : 'black')};
  padding: 6px 12px;
  font-size: 14px;
  border: none;
  border-radius: 4px;
  outline: none;
  // border: 1px solid ${(props) => (props.error ? 'red' : '#ccc')};
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
  font-size: 0.8rem;
  color: #333c44;
  font-weight: 600;
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
      <InputWrapper width={width}>
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
      </InputWrapper>
    </div>
  );
};

export default Select;
