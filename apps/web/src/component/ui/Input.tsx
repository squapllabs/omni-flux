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
  margin-bottom: 16px;
  width: ${(props) => props.width || '100%'};
`;

const StyledLabel = styled.label`
  margin-bottom: 8px;
  font-size: 0.875rem;
`;

const InputContainer = styled.div<StyledInputProps>`
  position: relative;
  display: flex;
  align-items: center;
  padding: ${(props) => `0 ${props.hasSuffixIcon ? '32px' : '12px'} 0 ${props.hasPrefixIcon ? '32px' : '12px'}`};
  border: 1px solid ${(props) => props.error ? 'red' : '#ccc'};
  border-radius: 4px;
  background-color: #f4f5f6;
  &:hover {
    border-color: #888;
  }
  &:focus-within {
    border-color: #007bff;
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

const PrefixIconWrapper = styled(IconWrapper)`
  position: absolute;
  left: 8px;
`;

const SuffixIconWrapper = styled(IconWrapper)`
  position: absolute;
  right: 8px;
`;

const InputError = styled.span`
  color: red;
  margin-top: 4px;
`;

const Input: React.FC<InputProps> = ({
  label,
  placeholder,
  error,
  width,
  prefixIcon,
  suffixIcon,
  ...props
}) => {
  return (
    <InputWrapper width={width}>
      {label && <StyledLabel>{label}</StyledLabel>}
      <InputContainer error={!!error} hasPrefixIcon={!!prefixIcon} hasSuffixIcon={!!suffixIcon}>
        {prefixIcon && <PrefixIconWrapper>{prefixIcon}</PrefixIconWrapper>}
        <StyledInput hasSuffixIcon={!!suffixIcon} placeholder={placeholder} {...props} />
        {suffixIcon && <SuffixIconWrapper>{suffixIcon}</SuffixIconWrapper>}
      </InputContainer>
      {error && <InputError>{error}</InputError>}
    </InputWrapper>
  );
};

export default Input;
