import React, { useState, useEffect, useRef } from 'react';
import Styles from '../../styles/vendorSelect.module.scss';
import MoreVerticalIcon from '../menu/icons/moreVerticalIcon';

const NewCustomMenu = ({ actions, name }: any) => {
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
    const handleClickOutside = (event: any) => {
      if (menuRef.current && !menuRef.current.contains(event.target)) {
        setIsOpen(false);
      }
    };
    document.addEventListener('mousedown', handleClickOutside);
    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
    };
  }, []);
  console.log('name', name);

  return (
    <div className={Styles.customMenu} ref={menuRef}>
      <span className={Styles.menuText} onClick={toggleMenu}>
        <MoreVerticalIcon />
      </span>
      {isOpen && (
        <div
          className={
            name === 'abstract'
              ? Styles.menuDropdownAbstract
              : name === 'BoQItems'
              ? Styles.menuDropdownItems
              : name === 'ApproveIndentList'
              ? Styles.menuApproveIndentList
              : Styles.menuDropdownOne
          }
        >
          {actions.map((action: any, index: any) => (
            <div
              key={index}
              // onClick={action.disabled ? null : action.onClick}
              onClick={() => handleItemClick(action)}
              // className={`${Styles.menuItem} ${
              //   action.disabled
              //     ? Styles.disabled
              //     : name === 'BoQItems' && action.disabled == true
              //     ? Styles.displayNone
              //     : ''
              // }`}
              className={
                name === 'BoQItems' && action.disabled == true
                  ? Styles.displayNone
                  : action.disabled
                  ? Styles.disabled
                  : Styles.menuItem
              }
            >
              {action.label}
            </div>
          ))}
        </div>
      )}
    </div>
  );
};

export default NewCustomMenu;
