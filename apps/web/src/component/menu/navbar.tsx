import React, { useState } from 'react';
import Styles from '../../styles/navbar.module.scss';
import { Link, NavLink } from 'react-router-dom';
import HomeIcon from './icons/homeIcon';
import DollarIcon from './icons/dollarIcon';
import SettingIcon from './icons/settingIcon';
import PlayIcon from './icons/playIcon';
import EnrichIcon from './icons/enrichIcon';
import SearchBar from './searchBar';
import Dropdown from './dropDown';
import Button from './button';
import CallIcon from './icons/callIcon';
import HelpIcon from './icons/help';
import AccountIcon from './icons/account';
import SecondaryNavbar from './secondaryNavbar';
import BellIcon from './icons/bellIcon';
import { useNavigate } from 'react-router-dom';
const Navbar = () => {
  const navigate = useNavigate();
  const [searchTerm, setSearchTerm] = useState('');
  function handleClickToAddAccount() {
    navigate('/add-products');
  }

  const handleSearch = (searchTerm: string) => {
    setSearchTerm(searchTerm);
    console.log(searchTerm);
  };

  const handleCallIconClick = () => {
    console.log('Call icon clicked');
  };

  const handleHelpIconClick = () => {
    console.log('Help icon clicked');
  };

  const handleNotificationsIconClick = () => {
    console.log('Notifications icon clicked');
  };

  const handleAccountIconClick = () => {
    console.log('Account icon clicked');
  };

  return (
    <div>
      <nav className={Styles.navbar}>
        <div className={Styles.logo}>
          {/* <img src="logo.png" alt="Logo" /> */}
          Logo
        </div>
        <div className={Styles.navLinks}>
          <Dropdown
            label={
              <div>
                <HomeIcon color="gray" className={Styles.navIcon} /> Home
              </div>
            }
          >
            <div className={Styles.dropdownContainer}>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
            </div>
          </Dropdown>

          <Dropdown
            label={
              <div>
                <HomeIcon color="gray" className={Styles.navIcon} /> Dashboard
              </div>
            }
          >
            <div className={Styles.dropdownContainer}>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
            </div>
          </Dropdown>

          <Dropdown
            label={
              <div>
                <HomeIcon color="gray" className={Styles.navIcon} /> Dashboard
              </div>
            }
          >
            <div className={Styles.dropdownContainer}>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
            </div>
          </Dropdown>

          <Dropdown
            label={
              <div>
                <HomeIcon color="gray" className={Styles.navIcon} /> Dashboard
              </div>
            }
          >
            <div className={Styles.dropdownContainer}>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
              <div className={Styles.dropdowntitle}>
                <div className={Styles.dropdownContent}>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Dolorem molestiae omnis.
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Doloremque, quas?
                  </p>
                  <p>
                    Lorem ipsum dolor sit amet consectetur adipisicing elit.
                    Rerum, odit!
                  </p>
                </div>
              </div>
            </div>
          </Dropdown>

          <Dropdown
            label={
              <div>
                <PlayIcon color="gray" className={Styles.navIcon} /> Product
              </div>
            }
          >
            <li>Submenu Item 1</li>
            <li>Submenu Item 2</li>
            <li>Submenu Item 3</li>
            <Button text="Add product" onClick={handleClickToAddAccount} />
          </Dropdown>
          <Dropdown
            label={
              <div>
                <DollarIcon color="gray" className={Styles.navIcon} /> Resources
              </div>
            }
          >
            <li>Submenu Item 1</li>
            <li>Submenu Item 2</li>
            <li>Submenu Item 3</li>
          </Dropdown>

          <Dropdown
            label={
              <div>
                <SettingIcon color="gray" className={Styles.navIcon} /> Setting
              </div>
            }
          >
            <li>Submenu Item 1</li>
            <li>Submenu Item 2</li>
            <li>Submenu Item 3</li>
          </Dropdown>
        </div>
        <div className={Styles.rightIcons}>
          <SearchBar onSearch={handleSearch} />

          <BellIcon
            className={Styles.navIcon}
            color="gray"
            height={24}
            width={24}
            onClick={handleNotificationsIconClick}
          />
          <AccountIcon
            className={Styles.navIcon}
            color="gray"
            height={24}
            width={24}
            onClick={handleAccountIconClick}
          />
        </div>
      </nav>
      {/* <SecondaryNavbar /> */}
    </div>
  );
};

export default Navbar;
