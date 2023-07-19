import React, { useState } from 'react';
import Styles from '..//..//styles/toggleSwitch.module.scss';

interface ToggleSwitchProps {
  onChange: (checked: boolean) => void;
}

const ToggleSwitch: React.FC<ToggleSwitchProps> = ({ onChange }) => {
  const [checked, setChecked] = useState(true);

  const handleToggle = () => {
    const newChecked = !checked;
    setChecked(newChecked);
    onChange(newChecked);
  };

  return (
    <label className={Styles.toggleSwitch}>
      <input type="checkbox" checked={checked} onChange={handleToggle} />
      <span className={Styles.toggleSlider} />
    </label>
  );
};

export default ToggleSwitch;
