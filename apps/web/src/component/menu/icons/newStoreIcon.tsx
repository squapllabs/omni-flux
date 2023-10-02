import React, { FC } from 'react';

interface EditIconProps {
  width?: number;
  height?: number;
  color?: string;
  onClick: () => void;
  style?: React.CSSProperties;
}

const NewStoreIcon: FC<EditIconProps> = ({
  width = 40,
  height = 40,
  color = '#475467',
  onClick,
  style,
  ...props
}) => {
  return (
    <div title="Store" onClick={onClick} style={style} role="button">
      <svg width="22" height="18" viewBox="0 0 22 18" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M21.5 5.01L20 0.51C19.9479 0.354764 19.8463 0.220877 19.7109 0.128786C19.5754 0.036695 19.4136 -0.00853652 19.25 1.05938e-08H2.75005C2.5865 -0.00853652 2.42465 0.036695 2.28922 0.128786C2.15379 0.220877 2.05223 0.354764 2.00005 0.51L0.500047 5.01C0.489242 5.08964 0.489242 5.17036 0.500047 5.25V9.75C0.500047 9.94891 0.579065 10.1397 0.719717 10.2803C0.86037 10.421 1.05114 10.5 1.25005 10.5H2.00005V18H3.50005V10.5H8.00005V18H20V10.5H20.75C20.949 10.5 21.1397 10.421 21.2804 10.2803C21.421 10.1397 21.5 9.94891 21.5 9.75V5.25C21.5109 5.17036 21.5109 5.08964 21.5 5.01ZM18.5 16.5H9.50005V10.5H18.5V16.5ZM20 9H17V6H15.5V9H11.75V6H10.25V9H6.50005V6H5.00005V9H2.00005V5.37L3.29005 1.5H18.71L20 5.37V9Z" fill="#7F56D9" />

     
      </svg>
    </div>
  );
};

export default NewStoreIcon;
