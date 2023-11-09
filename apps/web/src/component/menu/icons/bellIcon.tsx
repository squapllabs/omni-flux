import React from 'react';

interface BellIconProps {
  width?: number;
  height?: number;
  color?: string;
  onClick: () => void;
  className?: string;
  value?: number;
}

const BellIcon: React.FC<BellIconProps> = ({
  width = 30,
  height = 30,
  color = '#667085',
  className,
  onClick,
  value,
}) => {
  return (
    <div style={{ display: 'flex', alignItems: 'center' }}>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width={width}
        height={height}
        fill="none"
        viewBox="0 0 30 30"
        className={className}
        onClick={onClick}
      >
        <path
          stroke={color}
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth="1.667"
          d="M11.693 26.25A4.981 4.981 0 0 0 15 27.5a4.98 4.98 0 0 0 3.307-1.25M22.5 10a7.5 7.5 0 0 0-15 0c0 3.863-.974 6.508-2.063 8.257-.918 1.475-1.377 2.213-1.36 2.419.018.228.067.315.25.451.166.123.914.123 2.41.123h16.527c1.495 0 2.243 0 2.409-.123.183-.136.232-.223.25-.451.017-.206-.442-.944-1.36-2.42C23.474 16.509 22.5 13.864 22.5 10Z"
        />
      </svg>
      {value != null && (
        <div
          style={{
            position: 'relative',
            display: 'inline-block',
          }}
        >
          <div
            style={{
              position: 'absolute',
              top: '-25px',
              right: '0',
              backgroundColor: 'red',
              color: 'white',
              borderRadius: '50%',
              width: '18px',
              height: '18px',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }}
          >
            <div
              style={{
                fontWeight: 'bold',
                fontSize: value > 99 ? '7px' : '9px',
              }}
            >
              {value > 99 ? '99+' : value}
              {/* {'88'} */}
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default BellIcon;
