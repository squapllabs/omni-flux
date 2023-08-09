import React, { ReactElement } from 'react';

interface PreviousPageProps {
  width?: number;
  height?: number;
  color?: string;
  onClick?: () => void;
  style?: React.CSSProperties;
  disabled?: boolean;
}

const PreviousPageIcon: React.FC<PreviousPageProps> = ({
  width = 8,
  height = 14,
  color = '#000',
  onClick,
  style,
  disabled,
}: PreviousPageProps): ReactElement => {
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
      viewBox="0 0 8 14"
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
        d="M7 13 1 7l6-6"
      />
    </svg>
  );
};

export default PreviousPageIcon;
