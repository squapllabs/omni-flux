import React from 'react';
import styled from 'styled-components';

interface InputWrapperProps {
  width?: string;
}

interface StyledInputProps {
  error?: boolean;
  hasPrefixIcon?: boolean;
  hasSuffixIcon?: boolean;
}

interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  label?: string;
  placeholder?: string;
  error?: string;
  width?: string;
  prefixIcon?: React.ReactNode;
  suffixIcon?: React.ReactNode;
}

const InputWrapper = styled.div<InputWrapperProps>`
  display: flex;
  flex-direction: column;

  width: ${(props) => props.width || '100%'};
`;

const StyledLabel = styled.label`
  margin-bottom: 4px;
  font-size: 0.8rem;
  color: #333c44;
  font-weight: 600;
`;

const InputContainer = styled.div<StyledInputProps>`
  position: relative;
  display: flex;
  align-items: center;
  padding: ${(props) =>
    `0 ${props.hasSuffixIcon ? '32px' : '12px'} 0 ${
      props.hasPrefixIcon ? '32px' : '12px'
    }`};
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

const StyledInput = styled.input<StyledInputProps>`
  height: 34px;
  padding: ${(props) => `6px ${props.hasSuffixIcon ? '32px' : '0'} 6px 0`};
  border: none;
  background-color: transparent;
  flex-grow: 1;
  &:focus {
    outline: none;
  }
  box-sizing: border-box;
`;

const IconWrapper = styled.div`
  display: flex;
`;
const InputError = styled.span`
  color: red;
  margin-top: 2px;
  font-size: 0.75rem;
`;

const ErrorMessageWrapper = styled.div`
  min-height: 20px; // Change to the height of your error message
`;

const RequiredField = styled.span`
  color: red;
`;

const DatePicker: React.FC<InputProps & { mandatory?: boolean }> = ({
  label,
  placeholder,
  error,
  width,
  mandatory = false,
  ...props
}) => {
  const shouldShowAsterisk = mandatory;
  return (
    <InputWrapper width={width}>
      {label && (
        <StyledLabel>
          {label} {shouldShowAsterisk && <RequiredField>*</RequiredField>}
        </StyledLabel>
      )}
      <InputContainer error={!!error}>
        <StyledInput
          type="date" // Use the inputType here
          placeholder={placeholder}
          {...props}
        />
      </InputContainer>
      <ErrorMessageWrapper>
        {error && <InputError>{error}</InputError>}
      </ErrorMessageWrapper>
    </InputWrapper>
  );
};

export default DatePicker;
