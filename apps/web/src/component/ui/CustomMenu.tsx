import React, { useState, useEffect, useRef } from 'react';
import Styles from '../../styles/vendorSelect.module.scss';
import MoreHorizIcon from '../menu/icons/moreHorizontalIcon';

const CustomMenu = ({ actions }:any) => {
  const [isOpen, setIsOpen] = useState(false);
  const menuRef = useRef(null);

  const toggleMenu = () => {
    setIsOpen(!isOpen);
  };

  const handleItemClick = (action: any) => {
    if (!action.disabled) {
      action.onClick();
      setIsOpen(false); 
    }
  };


  useEffect(() => {
    const handleClickOutside = (event:any) => {
      if (menuRef.current && !menuRef.current.contains(event.target)) {
        setIsOpen(false);
      }
    };
    document.addEventListener('mousedown', handleClickOutside);
    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
    };
  }, []);

  return (
    <div className={Styles.customMenu} ref={menuRef}>
      <span className={Styles.menuText} onClick={toggleMenu}>
        <MoreHorizIcon />
      </span>
      {isOpen && (
        <div className={Styles.menuDropdown}>
          {actions.map((action:any, index:any) => (
            <div
            key={index}
            // onClick={action.disabled ? null : action.onClick}
            onClick={() => handleItemClick(action)}
            className={`${Styles.menuItem} ${action.disabled ? Styles.disabled : ''}`}
          >
              {action.label}
            </div>
          ))}
        </div>
      )}
    </div>
  );
};

export default CustomMenu;
