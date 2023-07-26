import React, { ReactElement } from 'react';

interface LastPageProps {
  width?: number;
  height?: number;
  color?: string;
  onClick?: () => void;
  style?: React.CSSProperties;
  disabled?: boolean;
}

const LastPageIcon: React.FC<LastPageProps> = ({
  width = 26,
  height = 24,
  color = '#000',
  onClick,
  style,
  disabled,
}: LastPageProps): ReactElement => {
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
      viewBox="0 0 26 24"
      style={{
        ...style,
        opacity: disabled ? 0.5 : 1,
        pointerEvents: disabled ? 'none' : 'auto',
      }}
      onClick={handleClick}
    >
      <path stroke={color} strokeWidth="1.5" d="M24.75 4v14" />
      <path
        stroke={color}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="2"
        d="m9 6 6 6-6 6"
      />
    </svg>
  );
};

export default LastPageIcon;
