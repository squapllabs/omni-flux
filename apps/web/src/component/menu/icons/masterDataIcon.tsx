import React, { FC } from 'react';

interface MasterDataIconProps {
  width?: number;
  height?: number;
  color?: string;
  // onClick: () => void;
  style?: React.CSSProperties;
}

const MasterDataIcon: FC<MasterDataIconProps> = ({
  width = 28,
  height = 28,
  color = '#475467',
  // onClick,
  style,
}) => {
  return (
    <div>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width={width}
        height={height}
        style={style}
        fill="none"
        viewBox="0 0 28 28"
        // onClick={onClick}
      >
        <path
          stroke={color}
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth="1.667"
          //   d="M4.667 12l6-6m-3 9l9-9-3-3-9 9v3h3z"
          d="M0.666992 0.666687H27.3337V27.3334H0.666992V0.666687ZM3.33366 3.33335V12.6667H5.99766V12.664H8.66966V12.6667H24.667V3.33335H3.33366ZM24.667 15.3334H8.66966V15.336H5.99766V15.3334H3.33366V24.6667H24.667V15.3334ZM5.99766 6.66669H8.66966V9.33869H5.99766V6.66669ZM5.99766 18.6667H8.66966V21.3387H5.99766V18.6667Z"
        />
      </svg>
    </div>
  );
};

export default MasterDataIcon;
