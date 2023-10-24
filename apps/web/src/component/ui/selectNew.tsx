import React, { FC, useState } from 'react';
import styled from 'styled-components';

interface SelectProps {
  onChange: (event: React.ChangeEvent<HTMLSelectElement>) => void;
  value: string;
  placeholder?: string;
  defaultLabel: string;
  width?: string;
  children: React.ReactNode;
  label?: string;
  name?: string;
  error?: boolean;
  helperText?: string;
  disabled?: boolean;
}
interface InputWrapperProps {
  width?: string;
}
interface StyledSelectProps {
  value: string;
  width?: string;
  error?: boolean;
  disabled?: boolean;
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
  background-color: ${(props) => (props.disabled ? '#FFFFFF' : '#FFFFFF')};
  pointer-events: ${(props) => (props.disabled ? 'none' : 'auto')};
  opacity: ${(props) => (props.disabled ? 0.7 : 1)};
  &:hover {
    border-color: ${(props) => (props.disabled ? '#ccc' : '#888')};
    cursor: ${(props) => (props.disabled ? 'not-allowed' : 'pointer')};
  }
  &:focus-within {
    outline: 0;
    box-shadow: ${(props) => (props.disabled ? 'none' : '0 0 0 2px #68717840')};
  }
`;
const StyledSelect = styled.select<StyledSelectProps>`
  appearance: none; // this is to remove default browser dropdown icon
  width: 100%;
  height: 35px;
  background-color: ${(props) => (props.disabled ? 'white' : 'transparent')};
  color: ${(props) => (props.disabled ? '#888' : 'inherit')};
  cursor: ${(props) => (props.disabled ? 'not-allowed' : 'pointer')};
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
    border-color: ${(props) => (props.disabled ? '#ccc' : '#888')};
    cursor: ${(props) => (props.disabled ? 'not-allowed' : 'pointer')};
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

const RequiredField = styled.span`
  color: red;
`;

const HelperText = styled.span`
  color: gray;
  margin-top: 2px;
  font-size: 0.75rem;
`;
const Select: FC<SelectProps & { mandatory?: boolean }> = ({
  onChange,
  label,
  value,
  defaultLabel,
  placeholder,
  width,
  children,
  name,
  error,
  disabled,
  mandatory = false,
  helperText = null,
}) => {
  const [isDropdownOpen, setIsDropdownOpen] = useState(false);
  const shouldShowAsterisk = mandatory;
  return (
    <div>
      <InputWrapper width={width}>
        {label && (
          <StyledLabel>
            {label} {shouldShowAsterisk && <RequiredField>*</RequiredField>}
          </StyledLabel>
        )}
        <div onClick={() => setIsDropdownOpen(!isDropdownOpen)}>
          <SelectContainer
            width={width}
            onClick={() => setIsDropdownOpen(!isDropdownOpen)}
            placeholder={placeholder}
          >
            <StyledSelect
              value={value}
              name={name}
              onChange={onChange}
              error={!!error}
              disabled={disabled}
              style={{
                color: `${value === '' ? 'gray' : ''}`,
              }}
            >
              {defaultLabel != null && <option value="">{placeholder}</option>}
              {children}
            </StyledSelect>
            <DropdownArrow
              onClick={() => {
                setIsDropdownOpen(!isDropdownOpen);
              }}
            />
          </SelectContainer>
        </div>
        <span>{helperText && <HelperText>Note:{helperText}</HelperText>}</span>
        {error === false ? (
          <></>
        ) : (
          <ErrorMessageWrapper>
            {error && <InputError>{error}</InputError>}
          </ErrorMessageWrapper>
        )}
      </InputWrapper>
    </div>
  );
};

export default Select;
