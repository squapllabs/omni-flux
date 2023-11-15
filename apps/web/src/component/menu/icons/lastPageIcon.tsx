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
      width="20"
      height="20"
      viewBox="0 0 20 20"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
      onClick={handleClick}
      style={{
        ...style,
        opacity: disabled ? 0.5 : 1,
        pointerEvents: disabled ? 'none' : 'auto',
      }}
    >
      <path
        d="M4.1665 9.99996H15.8332M15.8332 9.99996L9.99984 4.16663M15.8332 9.99996L9.99984 15.8333"
        stroke={color}
        strokeWidth="1.67"
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </svg>
  );
};

export default LastPageIcon;
