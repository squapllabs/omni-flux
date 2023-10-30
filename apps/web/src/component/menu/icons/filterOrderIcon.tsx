import React, { FC, CSSProperties } from 'react';

interface FilterIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: CSSProperties;
}

const FilterIcon: FC<FilterIconProps> = ({
  width = 25,
  height = 25,
  color = '#000',
  style,
}) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={width}
      height={height}
      viewBox="0 0 10 13"
    >
      <g
        fill="none"
        stroke="currentColor"
        stroke-linecap="round"
        stroke-linejoin="round"
      >
        {/* <rect width="13" height="13" x=".5" y=".5" rx="3" /> */}
        <path d="m10.5 8l-2 2V4m-5 2l2-2v6" />
      </g>
    </svg>
  );
};

export default FilterIcon;
