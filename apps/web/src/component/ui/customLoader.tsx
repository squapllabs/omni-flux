import React from 'react';
import CustomLoader from '../menu/icons/CustomSpinner';
import CustomSpineer from '../menu/icons/CustomSpinner';

interface PageLoaderProps {
  loading: boolean;
  size?: number;
  color?: string;
}

const PageLoader: React.FC<PageLoaderProps> = ({
  loading,
  size = 32,
  color = '#000',
  children,
}) => {
  const spinnerStyle: React.CSSProperties = {
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
    width: size,
    height: size,
    borderRadius: '50%',
    border: `${size / 8}px solid ${color}`,
    borderTop: `${size / 8}px solid transparent`,
    animation: 'spin 2s linear infinite',
  };
  return (
    <div
      style={{
        pointerEvents: loading ? 'none' : 'auto',
      }}
    >
      {loading && (
        <div
          style={{
            position: 'absolute',
            top: '50%',
            left: '50%',
            transform: 'translate(-50%, -50%)',
            padding: '16px',
            borderRadius: '8px',
            zIndex: 9999,
          }}
        >
          <div style={spinnerStyle}></div>
        </div>
      )}
      <div style={{ opacity: loading ? 0.5 : 1 }}>{children}</div>
    </div>
  );
};

export default PageLoader;
