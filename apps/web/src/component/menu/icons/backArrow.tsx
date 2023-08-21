import React, { SVGProps } from 'react';

interface KeyboardBackspaceIconProps extends SVGProps<SVGSVGElement> {
  color?: string;
  size?: number;
}

const KeyboardBackspaceIcon: React.FC<KeyboardBackspaceIconProps> = ({
  color = 'currentColor',
  size = 20,
  ...props
}) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={size}
    height={size}
    viewBox="0 0 20 20"
    fill="none"
    stroke={color}
    strokeWidth="1.5"
    strokeLinecap="round"
    strokeLinejoin="round"
    {...props}
  >
    <path d="M19 12H6M12 5l-7 7 7 7" />
  </svg>
);

export default KeyboardBackspaceIcon;
