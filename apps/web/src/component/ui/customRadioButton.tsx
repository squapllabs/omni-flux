import React from 'react';
import styled from 'styled-components';

interface RadioWrapperProps {
  disabled?: boolean;
}

interface RadioProps extends React.InputHTMLAttributes<HTMLInputElement> {
  label?: string;
  disabled?: boolean;
}

const RadioWrapper = styled.label<RadioWrapperProps>`
  display: flex;
  align-items: center;
  cursor: ${(props) => (props.disabled ? 'not-allowed' : 'pointer')};
  opacity: ${(props) => (props.disabled ? 0.7 : 1)};
  margin-right: 16px;
`;

const RadioInput = styled.input`
  margin: 0;
  cursor: pointer;
  opacity: 0;
  position: absolute;
  width: 0;
  height: 0;
  z-index: -1;
  &:checked + .radio-dot {
    background-color: #007bff;
    border: 2px solid #007bff;
  }
`;

const RadioLabel = styled.div`
  margin-left: 8px;
  font-size: 0.8rem;
  color: #333c44;
  font-weight: 600;
`;

const RadioDot = styled.div`
  width: 18px;
  height: 18px;
  border: 2px solid #ccc;
  border-radius: 50%;
  background-color: #fff;
  display: flex;
  justify-content: center;
  align-items: center;
`;

const Radio: React.FC<RadioProps> = ({ label, disabled, ...props }) => {
  return (
    <RadioWrapper disabled={disabled}>
      <RadioInput type="radio" {...props} />
      <RadioDot className="radio-dot" />
      {label && <RadioLabel>{label}</RadioLabel>}
    </RadioWrapper>
  );
};

export default Radio;
