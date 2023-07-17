import React, { useState, useEffect, useRef } from 'react';
import Styles from '../../styles/navbar.module.scss';

interface DropdownProps {
  label: React.ReactNode | string;
  children: React.ReactNode;
}

const Dropdown: React.FC<DropdownProps> = ({ label, children }) => {
  const [isOpen, setIsOpen] = useState(false);
  const dropdownRef = useRef<HTMLLIElement>(null);

  const toggleDropdown = () => {
    setIsOpen(!isOpen);
  };

  const handleDropdownClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    event.stopPropagation();
    toggleDropdown();
  };

  const handleClickOutside = (event: MouseEvent) => {
    if (
      dropdownRef.current &&
      !dropdownRef.current.contains(event.target as Node)
    ) {
      setIsOpen(false);
    }
  };

  useEffect(() => {
    document.addEventListener('click', handleClickOutside);

    return () => {
      document.removeEventListener('click', handleClickOutside);
    };
  }, []);

  return (
    <li
      className={`${Styles.dropdown} ${isOpen ? Styles.open : ''}`}
      onClick={toggleDropdown}
      ref={dropdownRef}
    >
      <button className={Styles.dropdownButton} onClick={handleDropdownClick}>
        {label}
      </button>
      {isOpen && <ul className={Styles.dropdownMenu}>{children}</ul>}
    </li>
  );
};

export default Dropdown;
