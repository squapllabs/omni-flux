import React, { FC } from 'react';

interface EditIconProps {
  width?: number;
  height?: number;
  color?: string;
  onClick: () => void;
  style?: React.CSSProperties;
}

const NewStoreIcon: FC<EditIconProps> = ({
  width = 22,
  height = 27,
  color = '#475467',
  onClick,
  style,
  ...props
}) => {
  return (
    <div title="Store">
      <svg
        width="40"
        height="40"
        viewBox="0 0 40 40"
        fill="none"
        xmlns="http://www.w3.org/2000/svg"
        {...props}
      >
        <path
          d="M12.5 15L15 11.6667H25L27.5 15M12.5 15V26.6667C12.5 27.1087 12.6756 27.5326 12.9882 27.8452C13.3007 28.1577 13.7246 28.3333 14.1667 28.3333H25.8333C26.2754 28.3333 26.6993 28.1577 27.0118 27.8452C27.3244 27.5326 27.5 27.1087 27.5 26.6667V15M12.5 15H27.5M23.3333 18.3333C23.3333 19.2174 22.9821 20.0652 22.357 20.6904C21.7319 21.3155 20.8841 21.6667 20 21.6667C19.1159 21.6667 18.2681 21.3155 17.643 20.6904C17.0179 20.0652 16.6667 19.2174 16.6667 18.3333"
          stroke="#667085"
          stroke-width="1.66667"
          stroke-linecap="round"
          stroke-linejoin="round"
        />
      </svg>
    </div>
  );
};

export default NewStoreIcon;
