import React, { CSSProperties, ReactNode } from 'react';

interface ButtonProps {
  height?: string | number;
  width?: string | number;
  text: string | ReactNode;
  backgroundColor?: string;
  textColor?: string;
  borderRadius?: string | number;

  fontSize?: string | number;
  fontWeight?: string | number;
  onClick: () => void;
  style?: CSSProperties;
  children?: ReactNode;
}

const Button: React.FC<ButtonProps> = ({
  height = '40px',
  width = '100px',
  text = 'Button',
  backgroundColor = '#1B92EB',
  textColor = 'white',
  borderRadius = '4px',

  fontSize = '14px',
  fontWeight = 'normal',
  onClick,
  style,
  children,
}) => {
  const buttonStyle: CSSProperties = {
    height,
    width,
    backgroundColor,
    color: textColor,
    borderRadius,

    fontSize,
    fontWeight,
    border: 'none',
    cursor: 'pointer',
    ...style,
  };

  return (
    <button style={buttonStyle} onClick={onClick}>
      {text}
      {children}
    </button>
  );
};

export default Button;
