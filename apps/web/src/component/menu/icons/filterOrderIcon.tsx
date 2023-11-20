import React, { FC, CSSProperties } from 'react';

interface FilterIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
}

const FilterOrderIcon: FC<FilterIconProps> = ({
  width = 20,
  height = 18,
  color = '#525151',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      viewBox="0 0 10 12"
      color={color}
    >
      <g
        fill="none"
        stroke="currentColor"
        strokeLinecap="round"
        strokeLinejoin="round"
      >
        <path d="m10.5 8l-2 2V4m-5 2l2-2v6" />
      </g>
    </svg>
  );
};

export default FilterOrderIcon;
