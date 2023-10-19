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
import DeliveryTruckIcon from './icons/deliveryTruckicon';
import BoxIcon from './icons/boxIcon';
import BookIcon from './icons/bookIcon';
import ReceptIcon from './icons/recepitIcon';
import PersonIcon from './icons/personIcon';

const Navbar = () => {
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const [searchTerm, setSearchTerm] = useState('');
  const [isMenuOpen, setIsMenuOpen] = useState(false);
  const [showAssets, setShowAssets] = useState(false);
  const [showHome, setShowHome] = useState(false);
  const [showProject, setShowProject] = useState(false);
  const [showResources, setShowResources] = useState(false);
  const [showReport, setShowReport] = useState(false);
  const menuRef = useRef<HTMLDivElement | null>(null);
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userData: any = encryptedData.userData;
  const roleName =
    encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
  const handleShowAssets = () => {
    setShowAssets(!showAssets);
    setShowResources(false);
    setShowProject(false);
    setShowReport(false);
  };
  const handleShowHome = () => {
    setShowHome(!showHome);
    setShowAssets(false);
    setShowResources(false);
    setShowProject(false);
    setShowReport(false);
  };
  const handleShowResources = () => {
    setShowResources(!showResources);
    setShowAssets(false);
    setShowProject(false);
    setShowReport(false);
  };
  const handleShowProject = () => {
    setShowProject(!showProject);
    setShowResources(false);
    setShowAssets(false);
    setShowReport(false);
  };
  const handleShowReport = () => {
    setShowProject(false);
    setShowResources(false);
    setShowAssets(false);
    setShowReport(!showReport);
  };
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
  const handleIndentapproval = () => {
    navigate('/indent-view');
  };
  const handlePurchaseOrder = () => {
    navigate('/purchase-order');
  };
  const handlePurchaseRequest = () => {
    navigate('/purchase-view');
  };
  const handleVendorList = () => {
    navigate('/vendor-list');
  };
  const handleExpenseApproval = () => {
    navigate('/site-expense-approve');
  };
  const handleFinanceView = () => {
    navigate('/finance-view');
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
        <div className={Styles.appContainer}>
          <div
            onClick={handleShowHome}
            className={
              showHome
                ? `${Styles.menu_item} ${Styles.selected}`
                : `${Styles.menu_item}`
            }
          >
            <div
              className={Styles.logo}
              onClick={() => {
                navigate('/home');
              }}
            >
              Omni ERP
            </div>
          </div>

          <div className={Styles.container}>
            <div className={Styles.verticalLine}></div>
          </div>
          <div className={Styles.navMenu}>
            <div
              onClick={handleShowAssets}
              className={
                showAssets
                  ? `${Styles.menu_item} ${Styles.selected}`
                  : `${Styles.menu_item}`
              }
            >
              <Dropdown
                label={
                  <div className={Styles.menuContainer}>
                    <DeliveryTruckIcon
                      color="white"
                      className={Styles.navIcon}
                    />
                    <div>
                      {roleName === 'FINANCE MANAGER'
                        ? 'Invoice '
                        : roleName === 'PLANNING ENGINEER'
                        ? 'Indent'
                        : 'Purchase'}
                    </div>

                    <DropdownIcon color="white" className={Styles.navIcon} />
                  </div>
                }
              >
                <div className={Styles.container}>
                  <div className={Styles.dropDownContainer}>
                    <p>
                      {roleName === 'FINANCE MANAGER'
                        ? 'Payment'
                        : roleName === 'PLANNING ENGINEER'
                        ? 'Indent Approval'
                        : 'Purchase'}
                    </p>
                    <div>
                      <div className={Styles.dropDownContent}>
                        {roleName === 'PLANNING ENGINEER' ||
                        roleName === 'PROJECT MANAGER' ||
                        roleName === 'ADMIN' ? (
                          <div
                            className={Styles.dropDownItems}
                            onClick={handleIndentapproval}
                          >
                            <div className={Styles.itemsTitle}>
                              {/* <CheckIcon /> */}
                              <h2>Indent Approval</h2>
                            </div>
                            <p>Manage your project indent</p>
                          </div>
                        ) : null}

                        {roleName === 'PURCHASE MANAGER' ||
                        roleName === 'PROJECT MANAGER' ||
                        roleName === 'ADMIN' ? (
                          <div
                            className={Styles.dropDownItems}
                            onClick={handlePurchaseOrder}
                          >
                            <div className={Styles.itemsTitle}>
                              {/* <CheckIcon /> */}
                              <h2>Purchase Order</h2>
                            </div>
                            <p>Manage your purchase order</p>
                          </div>
                        ) : null}

                        {roleName === 'PURCHASE MANAGER' ||
                        roleName === 'PROJECT MANAGER' ||
                        roleName === 'ADMIN' ? (
                          <div
                            className={Styles.dropDownItems}
                            onClick={handlePurchaseRequest}
                          >
                            <div className={Styles.itemsTitle}>
                              {/* <CheckIcon /> */}
                              <h2>Purchase Request</h2>
                            </div>
                            <p>Manage your purchase request</p>
                          </div>
                        ) : null}

                        {roleName === 'PROJECT MANAGER' ||
                        roleName === 'ADMIN' ? (
                          <div
                            className={Styles.dropDownItems}
                            onClick={handleExpenseApproval}
                          >
                            <div className={Styles.itemsTitle}>
                              {/* <CheckIcon /> */}
                              <h2>Expenses-Approval</h2>
                            </div>
                            <p>Manage your expenses approval</p>
                          </div>
                        ) : null}

                        {roleName === 'FINANCE MANAGER' ||
                        roleName === 'PROJECT MANAGER' ||
                        roleName === 'ADMIN' ? (
                          <div
                            className={Styles.dropDownItems}
                            onClick={handleFinanceView}
                          >
                            <div className={Styles.itemsTitle}>
                              {/* <CheckIcon /> */}
                              <h2>Invoice</h2>
                            </div>
                            <p>Manage your invoice and payments</p>
                          </div>
                        ) : null}

                        {roleName === 'PURCHASE MANAGER' ||
                        roleName === 'ADMIN' ? (
                          <div
                            className={Styles.dropDownItems}
                            onClick={handleVendorList}
                          >
                            <div className={Styles.itemsTitle}>
                              {/* <CheckIcon /> */}
                              <h2>Vendors</h2>
                            </div>
                            <p>Manage your approved vendor</p>
                          </div>
                        ) : null}
                      </div>
                    </div>
                  </div>
                </div>
              </Dropdown>
            </div>
            <div>
              <div
                onClick={handleShowResources}
                className={
                  showResources
                    ? `${Styles.menu_item} ${Styles.selected}`
                    : `${Styles.menu_item}`
                }
              >
                <Dropdown
                  label={
                    <div className={Styles.menuContainer}>
                      <BoxIcon color="white" />
                      Resources <DropdownIcon color="white" />
                    </div>
                  }
                >
                  <div className={Styles.container}>
                    <div className={Styles.dropDownContainer}>
                      <p>Resources</p>
                      <div>
                        <div className={Styles.dropDownContent}>
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
                              <p>
                                Learn how our customers are making big changes.
                              </p>
                            </div>
                          </div>
                          <div>
                            <div className={Styles.dropDownItems}>
                              <div className={Styles.itemsTitle}>
                                <CheckIcon />
                                <h2>Video tutorial</h2>
                              </div>
                              <p>
                                Get up and running on new features and
                                techniques.
                              </p>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                    <div className={Styles.dropDownContainer}>
                      <p>Company</p>
                      <div>
                        <div className={Styles.dropDownContent}>
                          <div className={Styles.dropDownItems}>
                            <div className={Styles.itemsTitle}>
                              <CheckIcon />
                              <h2>About us</h2>
                            </div>

                            <p>
                              Learn about our story and our mission statement.
                            </p>
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
                                We’re always looking for talented people. Join
                                our team!
                              </p>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </Dropdown>
              </div>
            </div>
            <div>
              <div
                onClick={handleShowReport}
                className={
                  showReport
                    ? `${Styles.menu_item} ${Styles.selected}`
                    : `${Styles.menu_item}`
                }
              >
                <div className={Styles.menuContainer}>
                  <ReceptIcon className={Styles.navIcon} />
                  Report
                </div>
              </div>
            </div>
            <div
              onClick={handleShowProject}
              className={
                showProject
                  ? `${Styles.menu_item} ${Styles.selected}`
                  : `${Styles.menu_item}`
              }
            >
              <div
                onClick={() => {
                  navigate('/project-list');
                }}
                className={Styles.menuContainer}
              >
                <BookIcon className={Styles.navIcon} />
                Project
              </div>
            </div>
          </div>
        </div>
        <div className={Styles.acountContainer}>
          <div className={Styles.rightSearch}>
            <SearchBar onSearch={handleSearch} />
          </div>
          <div className={Styles.container}>
            <div className={Styles.verticalLine}></div>
          </div>
          <div>
            <div className={Styles.rightIcons}>
              {/* <BellIcon
            className={Styles.navIcon}
            color="gray"
            height={24}
            width={24}
          /> */}
              <div ref={menuRef}>
                <PersonIcon
                  className={Styles.navIcon1}
                  color="white"
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
                    {roleName === 'ADMIN' ? (
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
                    ) : (
                      ''
                    )}
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
            </div>
          </div>
        </div>
      </nav>
    </div>
  );
};

export default Navbar;
