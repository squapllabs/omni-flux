import React, { useState, useEffect, useRef } from 'react';
import Styles from '../../styles/navbar.module.scss';
import HomeIcon from './icons/homeIcon';
import SettingIcon from './icons/settingIcon';
import SearchBar from './searchBar';
import Dropdown from './dropDown';
import AccountIcon from './icons/account';
import DropdownIcon from './icons/dropDownButton';
import BellIcon from './icons/bellIcon';
import CheckIcon from './icons/checkIcon';
import { useNavigate } from 'react-router-dom';
import { useDispatch } from 'react-redux';
import { resetAuth } from '../../redux/reducer';
import authService from '../../service/auth-service';
import LogoutIcon from './icons/logoutIcon';
import Avatar from './AvatarComponent';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';

const Navbar = () => {
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const [searchTerm, setSearchTerm] = useState('');
  const [isMenuOpen, setIsMenuOpen] = useState(false);
  const menuRef = useRef<HTMLDivElement | null>(null);
  const state: RootState = store.getState();
  let encryptedData = getToken(state, 'Data');
  let userData: any = encryptedData.userData;
  console.log('userData', userData);

  useEffect(() => {
    const handleOutsideClick = (event: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(event.target as Node)) {
        setIsMenuOpen(false);
      }
    };
    document.addEventListener('click', handleOutsideClick);
    return () => {
      document.removeEventListener('click', handleOutsideClick);
    };
  }, []);

  function handleListItems() {
    navigate('/products');
  }
  function handleLeadEnquires() {
    navigate('/lead-enquires');
  }

  const handleHomeRoute = () => {
    navigate('/home');
  };
  const handleSearch = (searchTerm: string) => {
    setSearchTerm(searchTerm);
  };

  const toggleMenu = () => {
    setIsMenuOpen((prevIsMenuOpen) => !prevIsMenuOpen);
  };

  const handleLogout = async () => {
    dispatch(resetAuth());
    const data = await authService.logout();
    if (data?.status === true) {
      navigate('/');
    }
  };

  const handleNavigate = () => {
    navigate('/settings');
  };

  return (
    <div>
      <nav className={Styles.navbar}>
        <div className={Styles.logo}>OmniFLUX ERP</div>
        <div className={Styles.navLinks}>
          <div onClick={handleHomeRoute}>
            <HomeIcon color="gray" className={Styles.navIcon} /> Home
          </div>

          <div>Dashboard</div>

          <Dropdown
            label={
              <div>
                Materials Management
                <DropdownIcon color="gray" />
              </div>
            }
          >
            <div className={Styles.container}>
              <div className={Styles.dropDownContainer}>
                <p>Use cases</p>
                <div>
                  <div className={Styles.dropDwonContent}>
                    <div className={Styles.dropDownItems}>
                      <div className={Styles.itemsTitle}>
                        <CheckIcon />
                        <h2>BBQ-Bill of Quantities</h2>
                      </div>

                      <p>
                        It's a document used in tendering in the construction
                        industry in which materials, parts, and labor (and their
                        costs) are itemized. It also provides details about the
                        conditions and specifications of the project.
                      </p>
                    </div>
                    <div onClick={handleListItems}>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Items</h2>
                        </div>
                        <p>
                          This would likely refer to the direct materials or
                          components that are used in the execution of a
                          project. This could range from cement, steel,
                          electrical components, and machinery parts etc., used
                          in construction.
                        </p>
                      </div>
                    </div>
                    <div>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Stocks</h2>
                        </div>
                        <p>
                          Find retention drivers and make your customers smile.
                        </p>
                      </div>
                    </div>
                  </div>
                </div>
              </div>

              <div className={Styles.dropDownContainer}>
                <p>Resources</p>
                <div>
                  <div className={Styles.dropDwonContent}>
                    <div
                      className={Styles.dropDownItems}
                      onClick={handleLeadEnquires}
                    >
                      <div className={Styles.itemsTitle}>
                        <CheckIcon />
                        <h2>Lead-Enquires</h2>
                      </div>
                      <p>The latest industry news, updates and info.</p>
                    </div>
                    <div>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Customer stories</h2>
                        </div>
                        <p>Learn how our customers are making big changes.</p>
                      </div>
                    </div>
                    <div>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Video tutorial</h2>
                        </div>
                        <p>
                          Get up and running on new features and techniques.
                        </p>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
              <div className={Styles.dropDownContainer}>
                <p>Company</p>
                <div>
                  <div className={Styles.dropDwonContent}>
                    <div className={Styles.dropDownItems}>
                      <div className={Styles.itemsTitle}>
                        <CheckIcon />
                        <h2>About us</h2>
                      </div>

                      <p>Learn about our story and our mission statement.</p>
                    </div>
                    <div>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Press</h2>
                        </div>
                        <p>
                          News and writings, press releases, and press
                          resources.
                        </p>
                      </div>
                    </div>
                    <div>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Careers</h2>
                        </div>
                        <p>
                          We’re always looking for talented people. Join our
                          team!
                        </p>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </Dropdown>

          {/* <Dropdown
            label={
              <div>
                <PlayIcon color="gray" className={Styles.navIcon} /> Product
              </div>
            }
          >
            <Button text="Add product" onClick={handleClickToAddAccount} />
          </Dropdown> */}
          <Dropdown
            label={
              <div>
                Resources <DropdownIcon color="gray" />
              </div>
            }
          >
            <div className={Styles.container}>
              <div className={Styles.dropDownContainer}>
                <p>Resources</p>
                <div>
                  <div className={Styles.dropDwonContent}>
                    <div className={Styles.dropDownItems}>
                      <div className={Styles.itemsTitle}>
                        <CheckIcon />
                        <h2>Blog</h2>
                      </div>

                      <p>The latest industry news, updates and info.</p>
                    </div>
                    <div>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Customer stories</h2>
                        </div>
                        <p>Learn how our customers are making big changes.</p>
                      </div>
                    </div>
                    <div>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Video tutorial</h2>
                        </div>
                        <p>
                          Get up and running on new features and techniques.
                        </p>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
              <div className={Styles.dropDownContainer}>
                <p>Company</p>
                <div>
                  <div className={Styles.dropDwonContent}>
                    <div className={Styles.dropDownItems}>
                      <div className={Styles.itemsTitle}>
                        <CheckIcon />
                        <h2>About us</h2>
                      </div>

                      <p>Learn about our story and our mission statement.</p>
                    </div>
                    <div>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Press</h2>
                        </div>
                        <p>
                          News and writings, press releases, and press
                          resources.
                        </p>
                      </div>
                    </div>
                    <div>
                      <div className={Styles.dropDownItems}>
                        <div className={Styles.itemsTitle}>
                          <CheckIcon />
                          <h2>Careers</h2>
                        </div>
                        <p>
                          We’re always looking for talented people. Join our
                          team!
                        </p>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </Dropdown>
        </div>
        <div className={Styles.rightIcons}>
          <SearchBar onSearch={handleSearch} />

          <BellIcon
            className={Styles.navIcon}
            color="gray"
            height={24}
            width={24}
          />
          <div ref={menuRef}>
            <AccountIcon
              className={Styles.navIcon}
              color="gray"
              height={24}
              width={24}
              onClick={toggleMenu}
            />
            {isMenuOpen && (
              <div className={Styles.menu}>
                <div className={Styles.box}>
                  <div className={Styles.profileDetail}>
                    <div>
                      <Avatar
                        firstName={userData?.first_name}
                        lastName={userData?.last_name}
                        size={40}
                      />
                    </div>
                    <div className={Styles.profileContents}>
                      <span className={Styles.profileName}>
                        {userData?.first_name} {userData?.last_name}
                      </span>
                      <span className={Styles.profileRole}>
                        {userData?.user_roles[0]?.role_data?.role_name}
                      </span>
                    </div>
                  </div>
                </div>
                <div className={Styles.box}>
                  <div>
                    <div
                      className={Styles.menubox}
                      onClick={() => handleNavigate()}
                    >
                      <SettingIcon />
                      <span>Settings</span>
                    </div>
                  </div>
                </div>
                <div className={Styles.box}>
                  <div>
                    <div
                      className={Styles.menubox}
                      onClick={() => handleLogout()}
                    >
                      <LogoutIcon style={{ fontWeight: 'bolder' }} />
                      <span>Logout</span>
                    </div>
                  </div>
                </div>
              </div>
            )}
          </div>
          <div></div>
        </div>
      </nav>
    </div>
  );
};

export default Navbar;
