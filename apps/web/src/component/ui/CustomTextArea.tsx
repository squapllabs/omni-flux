import React from 'react';
import styled from 'styled-components';

interface InputWrapperProps {
  width?: string;
}

interface TextAreaProps
  extends React.TextareaHTMLAttributes<HTMLTextAreaElement> {
  label?: string;
  placeholder?: string;
  error?: string;
  width?: string;
  rows?: number; // Added rows prop for setting the number of rows
}

const InputWrapper = styled.div<InputWrapperProps>`
  display: flex;
  flex-direction: column;
  height: 100px;
  width: ${(props) => props.width || '100%'};
`;

const StyledLabel = styled.label`
  margin-bottom: 4px;
  font-size: 0.8rem;
  color: #333c44;
  font-weight: 600;
`;

const StyledTextArea = styled.textarea<TextAreaProps>`
  height: ${(props) =>
    props.rows
      ? `${props.rows * 15}px`
      : '100%'}; /* Set height based on the number of rows */
  //   padding: 10px; /* Adjust as needed */
  border: 1px solid ${(props) => (props.error ? 'red' : '#ccc')};
  border-radius: 4px;
  resize: vertical; /* Allows vertical resizing */
  background-color: #f4f5f6;
  &:hover {
    border-color: #888;
  }
  &:focus {
    outline: none;
    box-shadow: 0 0 0 2px #68717840;
  }
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
  min-height: 20px;
`;

const TextArea: React.FC<TextAreaProps> = ({
  label,
  placeholder,
  error,
  width,
  rows, // Add rows prop here
  ...props
}) => {
  return (
    <InputWrapper width={width}>
      {label && <StyledLabel>{label}</StyledLabel>}
      {/* Pass rows prop to StyledTextArea */}
      <StyledTextArea
        error={!!error}
        placeholder={placeholder}
        rows={rows}
        {...props}
      />
      <ErrorMessageWrapper>
        {error && <InputError>{error}</InputError>}
      </ErrorMessageWrapper>
    </InputWrapper>
  );
};

export default TextArea;
