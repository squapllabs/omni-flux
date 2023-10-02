import React, { FC } from 'react';

interface MasterDataIconProps {
  width?: number;
  height?: number;
  color?: string;
  style?: React.CSSProperties;
}

const MasterDataIcon: FC<MasterDataIconProps> = ({
  width = 28,
  height = 28,
  color = '#475467',
  style,
}) => {
  return (
    <div>
      <svg
        width="28"
        height="28"
        viewBox="0 0 32 32"
        fill="none"
        xmlns="http://www.w3.org/2000/svg"
      >
        <path
          d="M2.66699 2.66669H29.3337V29.3334H2.66699V2.66669ZM5.33366 5.33335V14.6667H7.99766V14.664H10.6697V14.6667H26.667V5.33335H5.33366ZM26.667 17.3334H10.6697V17.336H7.99766V17.3334H5.33366V26.6667H26.667V17.3334ZM7.99766 8.66669H10.6697V11.3387H7.99766V8.66669ZM7.99766 20.6667H10.6697V23.3387H7.99766V20.6667Z"
          fill="black"
        />
      </svg>
    </div>
  );
};

export default MasterDataIcon;
