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
`;

const StyledInput = styled.input<StyledInputProps>`
  height: 24px;
  padding-left: ${(props) => (props.hasPrefixIcon ? '32px' : '8px')};
  padding-right: ${(props) => (props.hasSuffixIcon ? '32px' : '8px')};
  border: 1px solid ${(props) => (props.error ? 'red' : '#ccc')};
  &:hover {
    border-color: #888;
  }
  &:focus {
    outline: none;
    border-color: #007bff;
  }
`;

const IconWrapper = styled.div`
  position: absolute;
  top: calc(50% - 8px);
`;

const PrefixIconWrapper = styled(IconWrapper)`
  left: 8px;
`;

const SuffixIconWrapper = styled(IconWrapper)`
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
      <div style={{ position: 'relative' }}>
        {prefixIcon && <PrefixIconWrapper>{prefixIcon}</PrefixIconWrapper>}
        <StyledInput 
          placeholder={placeholder} 
          error={!!error} 
          hasPrefixIcon={!!prefixIcon}
          hasSuffixIcon={!!suffixIcon} 
          {...props} 
        />
        {suffixIcon && <SuffixIconWrapper>{suffixIcon}</SuffixIconWrapper>}
      </div>
      {error && <InputError>{error}</InputError>}
    </InputWrapper>
  );
};

export default Input;
