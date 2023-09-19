import React, { ReactNode } from 'react';

interface DisabledProps {
  disabled: boolean;
  children: ReactNode;
}

const PageDisabled: React.FC<DisabledProps> = ({ disabled, children }) => {
  if (disabled) {
    return (
      <div style={{ opacity: 0.6, pointerEvents: 'none' }}>{children}</div>
    );
  }
  return <>{children}</>;
};

export default PageDisabled;
