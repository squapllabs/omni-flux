// Category.tsx
import React from 'react';
import Styles from '../../styles/submenu.module.scss';

interface MenuItem {
  id: number;
  name: string;
}

interface SideNavProps {
  menuItems: MenuItem[];
  selectedItem: number; // Add the new prop for selectedItem
  handleMenuItemClick: (id: number) => void;
}

const SideNav: React.FC<SideNavProps> = ({
  menuItems,
  selectedItem, // Receive selectedItem from props
  handleMenuItemClick,
}) => {
  const onItemClick = (id: number) => {
    handleMenuItemClick(id); // Call the parent function to handle the click action if needed.
  };
  return (
    <div className={Styles.Container}>
      <div className={Styles.menu}>
        <ul>
          {menuItems.map((item) => (
            <li
              key={item.id}
              className={selectedItem === item.id ? Styles.selected : ''}
              onClick={() => onItemClick(item.id)}
            >
              {item.name}
            </li>
          ))}
        </ul>
      </div>
    </div>
  );
};

export default SideNav;
