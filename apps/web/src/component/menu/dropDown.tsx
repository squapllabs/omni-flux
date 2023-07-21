import React, { useState, useEffect, useRef } from 'react';
import Styles from '../../styles/dropdown.module.scss';

interface DropdownProps {
  label: React.ReactNode | string;
  children: React.ReactNode;
}

const Dropdown: React.FC<DropdownProps> = ({ label, children }) => {
  const [isOpen, setIsOpen] = useState(false);
  const dropdownRef = useRef<HTMLDivElement>(null);

  const toggleDropdown = () => {
    setIsOpen(!isOpen);
  };

  const handleDropdownClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    // event.stopPropagation();
    toggleDropdown();
  };

  useEffect(() => {
    document.addEventListener('click', handleClickOutside);

    return () => {
      document.removeEventListener('click', handleClickOutside);
    };
  }, []);

  const handleClickOutside = (event: MouseEvent) => {
    if (
      dropdownRef.current &&
      !dropdownRef.current.contains(event.target as Node)
    ) {
      setIsOpen(false);
    }
  };

  return (
    <div
      className={`${Styles.dropdown} ${isOpen ? Styles.open : ''}`}
      onClick={toggleDropdown}
      ref={dropdownRef}
    >
      <button className={Styles.dropdownButton} onClick={handleDropdownClick}>
        {label}
      </button>
      {isOpen && <ul className={Styles.dropdownMenu}>{children}</ul>}
    </div>
  );
};

export default Dropdown;
