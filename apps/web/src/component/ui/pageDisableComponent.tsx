import React, { ReactNode } from 'react';

interface DisabledProps {
  disabled: boolean;
  children: ReactNode;
}

const PageDisabled: React.FC<DisabledProps> = ({ disabled, children }) => {
  if (disabled) {
    return <div style={{ pointerEvents: 'none' }}>{children}</div>;
  }
  return <div>{children}</div>;
};

export default PageDisabled;
