import React, { ReactElement } from 'react';

interface FirstPageProps {
  width?: number;
  height?: number;
  color?: string;
  onClick?: () => void;
  style?: React.CSSProperties;
  disabled?: boolean;
}

const FirstPageIcon: React.FC<FirstPageProps> = ({
  width = 24,
  height = 24,
  color = '#000',
  onClick,
  style,
  disabled,
}: FirstPageProps): ReactElement => {
  const handleClick = () => {
    if (!disabled && onClick) {
      onClick();
    }
  };

  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      fill="none"
      viewBox="0 0 24 24"
      style={{
        ...style,
        opacity: disabled ? 0.5 : 1,
        pointerEvents: disabled ? 'none' : 'auto',
      }}
      onClick={handleClick}
    >
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="2"
        d="m15 18-6-6 6-6"
      />
      <path stroke={color} strokeWidth="1.5" d="M.75 5v14" />
    </svg>
  );
};

export default FirstPageIcon;
