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
    <svg width="14" height="14" viewBox="0 0 14 14" fill="none" xmlns="http://www.w3.org/2000/svg" onClick={handleClick} style={{
      ...style,
      opacity: disabled ? 0.5 : 1,
      pointerEvents: disabled ? 'none' : 'auto',
    }}>
      <path d="M12.8332 6.99996H1.1665M1.1665 6.99996L6.99984 12.8333M1.1665 6.99996L6.99984 1.16663" stroke={color} stroke-width="1.67" stroke-linecap="round" stroke-linejoin="round" />
    </svg>
  );
};

export default FirstPageIcon;
