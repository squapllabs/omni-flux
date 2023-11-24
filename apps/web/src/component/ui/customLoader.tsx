import React from 'react';

interface PageLoaderProps {
  loading: boolean;
  size?: number;
  color?: string;
  children?:any;
}

const CustomLoader: React.FC<PageLoaderProps> = ({
  loading,
  size = 20,
  color = '#000',
  children,
}) => {
  const spinnerStyle: React.CSSProperties = {
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
    width: 38,
    height: 38,
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

export default CustomLoader;
const styles = `
@keyframes spin {
  0% {
    transform: rotate(0deg);
  }
  100% {
    transform: rotate(360deg);
  }
}
`;

// Insert the styles into the document head
const styleElement = document.createElement('style');
styleElement.appendChild(document.createTextNode(styles));
document.head.appendChild(styleElement);
