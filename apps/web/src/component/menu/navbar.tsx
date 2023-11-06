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
import MenuIcon from './icons/hamburgerIcon';
import CloseIcon from '../menu/icons/closeIcon';
import {
  getNewNotificationByUserID,
  getNotificationforUser,
  updateNotificationStatus,
} from '../../hooks/notification-hooks';
import { format } from 'date-fns';
import StarIcon from './icons/starIcon';
import notificationService from '../../service/notification-service';

const Navbar = () => {
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const [searchTerm, setSearchTerm] = useState('');
  const [isMenuOpen, setIsMenuOpen] = useState(false);
  const [notificationOpen, setNotificationOpen] = useState(false);
  const [unread, setUnread] = useState<any>(null);
  const [showAssets, setShowAssets] = useState(false);
  const [showHome, setShowHome] = useState(false);
  const [showProject, setShowProject] = useState(false);
  const [showResources, setShowResources] = useState(false);
  const [showReport, setShowReport] = useState(false);
  const menuRef = useRef<HTMLDivElement | null>(null);
  const [isCollapsed, setIsCollapsed] = useState<boolean>(true);
  const [isResourceCollapsed, setIsResourceCollapsed] = useState<boolean>(true);
  const [isMenuIconOpen, setIsMenuIconOpen] = useState(false);
  const [screenSize, setScreenSize] = useState(getCurrentDimension());
  const [refreshed, setRefreshed] = useState(false);
  const notificationRef = useRef<HTMLDivElement | null>(null);
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userData: any = encryptedData.userData;
  const roleName =
    encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
  const profile: any =
    encryptedData?.userData?.user_profiles?.profile_image_url;

  const handleMenuIcon = () => {
    setIsMenuIconOpen(!isMenuIconOpen);
  };

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
  const { mutate: updateNotification } = updateNotificationStatus();

  const { data: newNotificationCount } = getNewNotificationByUserID(
    userData?.user_id
  );

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

  useEffect(() => {
    const handleOutsideClick = (event: MouseEvent) => {
      if (
        notificationRef.current &&
        !notificationRef.current.contains(event.target as Node)
      ) {
        setNotificationOpen(false);
        setUnread(false);
      }
    };
    document.addEventListener('click', handleOutsideClick);
    return () => {
      document.removeEventListener('click', handleOutsideClick);
    };
  }, []);

  console.log('userData', userData);

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
  const handleExpenseRecall = () => {
    navigate('/expense-recall');
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

  const data: any = {
    limit: 5,
    offset: 0,
    order_by_column: 'created_date',
    order_by_direction: 'desc',
    notification_to_user_id: userData?.user_id,
  };

  const {
    isLoading: dataLoading,
    data: initialData,
    refetch,
  } = getNotificationforUser(data);

  useEffect(() => {
    refetch();
  }, [notificationOpen]);

  const handleOpen = () => {
    setNotificationOpen((prevNotificationOpen) => !prevNotificationOpen);
    setUnread(true);
  };

  const handleLogout = async () => {
    const data = await authService.logout();
    if (data?.status === true) {
      dispatch(resetAuth());
      navigate('/');
    }
  };

  const handleNavigate = () => {
    navigate('/settings');
  };

  function getCurrentDimension() {
    return {
      width: window.innerWidth,
      height: window.innerHeight,
    };
  }

  useEffect(() => {
    const updateDimension = () => {
      setScreenSize(getCurrentDimension());
    };
    window.addEventListener("resize", updateDimension);

    return () => {
      window.removeEventListener("resize", updateDimension);
    };
  }, [screenSize]);

  const toggleCollapse = () => {
    setIsCollapsed(!isCollapsed);
  };

  const resourceToggleCollapse = () => {
    setIsResourceCollapsed(!isResourceCollapsed);
  }
  const notificationFunction = () => {
    handleOpen();
    setTimeout(() => {
      upadteReadStatus();
    }, 2000);
  };

  const upadteReadStatus = () => {
    const Object: any = {
      notification_to_user_id: userData?.user_id,
    };
    updateNotification(Object, {
      onSuccess: (data) => {
        if (data?.status === true) {
          refetch();
        }
      },
    });
  };

  return (
    <div>
      {screenSize.width > 750 && (
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
                    roleName === 'HR' || roleName === 'ADMIN' ? (
                      <div className={Styles.menuContainer}>
                        <BoxIcon color="white" />
                        Resources <DropdownIcon color="white" />
                      </div>
                    ) : null
                  }
                >
                  <div className={Styles.container}>
                    <div className={Styles.dropDownContainer}>
                      {/* <p>
                        {roleName === 'FINANCE MANAGER'
                          ? ''
                          : roleName === 'PLANNING ENGINEER'
                            ? 'Purchase'
                            : 'Purchase'}
                      </p> */}
                      <div>
                        <div className={Styles.dropDownContent}>
                          {roleName === 'PLANNING ENGINEER' ||
                            roleName === 'PROJECT MANAGER' ? (
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
                        </div>
                      </div>
                    </div>
                    <div className={Styles.dropDownContainer}>
                      {/* <p>
                        {roleName === 'FINANCE MANAGER' || roleName === 'ADMIN' ? 'Expenses' : ''}
                      </p> */}
                      <div>
                        <div className={Styles.dropDownContent}>
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

                          {roleName === 'PROJECT MANAGER' ||
                            roleName === 'ADMIN' ? (
                            <div
                              className={Styles.dropDownItems}
                              onClick={handleExpenseRecall}
                            >
                              <div className={Styles.itemsTitle}>
                                {/* <CheckIcon /> */}
                                <h2>Expense-Reversal</h2>
                              </div>
                              <p>Manage your expense recall</p>
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
                        </div>
                      </div>
                    </div>
                    <div className={Styles.dropDownContainer}>
                      {/* <p>
                        {
                          roleName === 'FINANCE MANAGER' || roleName === 'PLANNING ENGINEER' 
                          ? '' : 'Vendors'
                        }
                      </p> */}
                      <div>
                        <div className={Styles.dropDownContent}>
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
                  <div className={Styles.menuContainer} onClick={() => {
                    navigate('/reports');
                  }}>
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
              {/* <SearchBar onSearch={handleSearch} /> */}
            </div>
            <div>
              <div ref={notificationRef} onClick={notificationFunction}>
                <div>
                  <BellIcon
                    color="white"
                    width={25}
                    height={25}
                    value={newNotificationCount}
                  />
                </div>
                {notificationOpen && (
                  <div className={Styles.menu1}>
                    {initialData?.content?.map((data: any, index: any) => {
                      const currentDate: any = new Date();
                      const applied_date: any = new Date(data?.created_date);
                      const timeDifference: any = currentDate - applied_date;
                      const minutes = Math.floor(timeDifference / (1000 * 60));
                      const hours = Math.floor(minutes / 60);
                      const days = Math.floor(
                        timeDifference / (24 * 60 * 60 * 1000)
                      );
                      return (
                        <div>
                          <div className={Styles.box}>
                            {data?.is_read === false ? (
                              <div className={Styles.menubox1}>
                                <Avatar
                                  imageUrl={
                                    data?.notification_from_user_data
                                      ?.user_profiles?.profile_image_url
                                  }
                                  firstName={
                                    data?.notification_from_user_data?.first_name
                                  }
                                  lastName={
                                    data?.notification_from_user_data?.last_name
                                  }
                                  size={25}
                                  fontSizeChange={true}
                                />
                                <span className={Styles.lineHeading}>
                                  {data?.notification_from_user_data?.first_name +
                                    ' ' +
                                    data?.notification_from_user_data?.last_name}
                                </span>
                                <span>
                                  <StarIcon />
                                </span>
                              </div>
                            ) : (
                              <div className={Styles.menubox1}>
                                <Avatar
                                  imageUrl={
                                    data?.notification_from_user_data
                                      ?.user_profiles?.profile_image_url
                                  }
                                  firstName={
                                    data?.notification_from_user_data?.first_name
                                  }
                                  lastName={
                                    data?.notification_from_user_data?.last_name
                                  }
                                  size={25}
                                  fontSizeChange={true}
                                />
                                <span className={Styles.lineHeading}>
                                  {data?.notification_from_user_data?.first_name +
                                    ' ' +
                                    data?.notification_from_user_data?.last_name}
                                </span>
                              </div>
                            )}
                            <div className={Styles.messages}>
                              {data?.notification_type === 'Indent-Requested' ? (
                                <span className={Styles.lineContent}>
                                  has requested an Indent
                                </span>
                              ) : (
                                <span className={Styles.lineContent}>
                                  has approved an Indent
                                </span>
                              )}
                              {timeDifference < 24 * 60 * 60 * 1000 ? (
                                <span className={Styles.lineTime}>
                                  {` ${hours} hours ${minutes % 60} minutes ago`}
                                </span>
                              ) : (
                                <span className={Styles.lineTime}>
                                  {`${days} days ago`}
                                </span>
                              )}
                            </div>
                            <div className={Styles.dividerStyle}></div>
                          </div>
                        </div>
                      );
                    })}
                  </div>
                )}
              </div>
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
                <div ref={menuRef} onClick={toggleMenu}>
                  <div className={Styles.profileDetailHead}>
                    {/* <PersonIcon
                      className={Styles.navIcon1}
                      color="white"
                      height={24}
                      width={24}
                    /> */}
                    <Avatar
                      imageUrl={profile}
                      firstName={userData?.first_name}
                      lastName={userData?.last_name}
                      size={40}
                    />
                    <div
                      className={Styles.profileContents}
                      style={{ color: 'white' }}
                    >
                      <span className={Styles.profileName}>
                        {userData?.first_name + ' ' + userData?.last_name}
                      </span>
                      <span className={Styles.profileRole}>
                        {userData?.user_roles[0]?.role_data?.role_name}
                      </span>
                    </div>
                  </div>

                  {isMenuOpen && (
                    <div className={Styles.menu}>
                      {/* <div className={Styles.box}>
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
                      </div> */}
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
      )}
      {screenSize.width <= 750 && (
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
          </div>
          <div className={Styles.menuIcon} onClick={() => { handleMenuIcon() }}>
            <MenuIcon />
          </div>
          {
            isMenuIconOpen ? (
              <div>
                <div className={Styles.popupContainer}>
                  <div className={Styles.popupContent}>
                    <div className={Styles.leftdialogStyle}></div>
                    <div className={Styles.dialogStyle}>
                      <div className={Styles.boxStyle}>
                        <div className={Styles.mainContent}>
                          <div className={Styles.popupHeader}>
                            {/* <div className={Styles.textContent_1}>
                          <h4>Vertical Nav Bar</h4>
                          <span className={Styles.content}></span>
                        </div> */}
                            <div className={Styles.closeMenuIcon} onClick={() => { handleMenuIcon() }}>
                              <MenuIcon />
                            </div>
                          </div>
                        </div>
                        <div className={Styles.main_content}>
                          <div className={Styles.navigationLinks}>
                            <div>
                              <div onClick={toggleCollapse} style={{ cursor: 'pointer' }}>
                                <div className={Styles.verticalMenuContainer} data-toggle="collapse" data-target="#demo">
                                  <DeliveryTruckIcon
                                    color="black"
                                    className={Styles.navIcon}
                                  />
                                  <div>
                                    {roleName === 'FINANCE MANAGER'
                                      ? 'Invoice '
                                      : roleName === 'PLANNING ENGINEER'
                                        ? 'Indent'
                                        : 'Procurement'}
                                  </div>

                                  <DropdownIcon color="black" className={Styles.navIcon} />
                                </div>
                              </div>
                              <div id="demo" style={{
                                maxHeight: isCollapsed ? '0' : 'none', // If collapsed, set maxHeight to 0, else set to none for natural height
                                overflow: 'hidden', // Hide the overflow when collapsed
                                transition: 'max-height 0.5s ease', // Transition effect for the collapsing action
                                padding: '6px 0px 0px 20px',
                                // backgroundColor: 'green'
                              }}>
                                <div className={Styles.dropDownContainer}>
                                  {/* <p>
                                  {roleName === 'FINANCE MANAGER'
                                    ? ''
                                    : roleName === 'PLANNING ENGINEER'
                                      ? 'Purchase'
                                      : 'Purchase'}
                                </p> */}
                                  <div>
                                    <div className={Styles.dropDownContent} id="demo" class="collapse">
                                      {roleName === 'PLANNING ENGINEER' ||
                                        roleName === 'PROJECT MANAGER' ? (
                                        <div
                                          className={Styles.dropDownItems}
                                          onClick={handleIndentapproval}
                                        >
                                          <div className={Styles.itemsTitle}>
                                            <h2>Indent Approval</h2>
                                          </div>
                                          <p>Manage your project indent</p>
                                        </div>
                                      ) : null}
                                      <div className={Styles.dropDownContent}>

                                        {roleName === 'PURCHASE MANAGER' ||
                                          roleName === 'PROJECT MANAGER' ||
                                          roleName === 'ADMIN' ? (
                                          <div
                                            className={Styles.dropDownItems}
                                            onClick={handlePurchaseOrder}
                                          >
                                            <div className={Styles.itemsTitle}>
                                              <h2>Purchase Order</h2>
                                            </div>
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
                                              <h2>Purchase Request</h2>
                                            </div>
                                          </div>
                                        ) : null}
                                      </div>
                                    </div>
                                  </div>
                                </div>
                                <div className={Styles.dropDownContainer}>
                                  {/* <p>
                                  {roleName === 'FINANCE MANAGER' || roleName === 'ADMIN' ? 'Expenses' : ''}
                                </p> */}
                                  <div>
                                    <div className={Styles.dropDownContent}>
                                      {roleName === 'PROJECT MANAGER' ||
                                        roleName === 'ADMIN' ? (
                                        <div
                                          className={Styles.dropDownItems}
                                          onClick={handleExpenseApproval}
                                        >
                                          <div className={Styles.itemsTitle}>
                                            <h2>Expenses-Approval</h2>
                                          </div>
                                        </div>
                                      ) : null}

                                      {roleName === 'PROJECT MANAGER' ||
                                        roleName === 'ADMIN' ? (
                                        <div
                                          className={Styles.dropDownItems}
                                          onClick={handleExpenseRecall}
                                        >
                                          <div className={Styles.itemsTitle}>
                                            <h2>Expense-Reversal</h2>
                                          </div>
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

                                            <h2>Invoice</h2>
                                          </div>
                                        </div>
                                      ) : null}
                                    </div>
                                  </div>
                                </div>
                                <div className={Styles.dropDownContainer}>
                                  {/* <p>
                                  {
                                    roleName === 'FINANCE MANAGER' || roleName === 'PLANNING ENGINEER'
                                      ? '' : 'Vendors'
                                  }
                                </p> */}
                                  <div>
                                    <div className={Styles.dropDownContent}>
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
                                        </div>
                                      ) : null}
                                    </div>
                                  </div>
                                </div>
                              </div>

                            </div>
                            <div>
                              <div
                                onClick={handleShowResources}
                              >
                                <div onClick={resourceToggleCollapse} style={{ cursor: 'pointer' }}>
                                  <div className={Styles.verticalMenuContainer} data-toggle="collapse" data-target="#demo">
                                    <BoxIcon
                                      color="black"
                                      className={Styles.navIcon}
                                    />
                                    <div>
                                      Resources
                                    </div>
                                    <DropdownIcon color="black" className={Styles.navIcon} />
                                  </div>
                                </div>
                                <div id="demo" style={{
                                  maxHeight: isResourceCollapsed ? '0' : 'none', // If collapsed, set maxHeight to 0, else set to none for natural height
                                  overflow: 'hidden', // Hide the overflow when collapsed
                                  transition: 'max-height 0.5s ease', // Transition effect for the collapsing action
                                  padding: '6px 0px 0px 20px',
                                  // backgroundColor: 'green'
                                }}>
                                  <div className={Styles.dropDownContainer}>
                                    <div>
                                      <div className={Styles.dropDownContent} id="demo" class="collapse">
                                        <div>
                                          <div className={Styles.dropDownContent}>
                                            <div className={Styles.dropDownItems}>
                                              <div className={Styles.itemsTitle}>
                                                <h2>Blog</h2>
                                              </div>
                                              {/* <p>The latest industry news, updates and info.</p> */}
                                            </div>
                                            <div>
                                              <div className={Styles.dropDownItems}>
                                                <div className={Styles.itemsTitle}>
                                                  <h2>Customer stories</h2>
                                                </div>
                                                {/* <p>
                                            Learn how our customers are making big changes.
                                          </p> */}
                                              </div>
                                            </div>
                                            <div>
                                              <div className={Styles.dropDownItems}>
                                                <div className={Styles.itemsTitle}>
                                                  <h2>Video tutorial</h2>
                                                </div>
                                                {/* <p>
                                            Get up and running on new features and
                                            techniques.
                                          </p> */}
                                              </div>
                                            </div>
                                          </div>
                                        </div>
                                        <div className={Styles.dropDownContainer}>
                                          {/* <p>Company</p> */}
                                          <div>
                                            <div className={Styles.dropDownContent}>
                                              <div className={Styles.dropDownItems}>
                                                <div className={Styles.itemsTitle}>
                                                  <h2>About us</h2>
                                                </div>

                                                {/* <p>
                                            Learn about our story and our mission statement.
                                             </p> */}
                                              </div>
                                              <div>
                                                <div className={Styles.dropDownItems}>
                                                  <div className={Styles.itemsTitle}>
                                                    <h2>Press</h2>
                                                  </div>
                                                  {/* <p>
                                            News and writings, press releases, and press
                                            resources.
                                          </p> */}
                                                </div>
                                              </div>
                                              <div>
                                                <div className={Styles.dropDownItems}>
                                                  <div className={Styles.itemsTitle}>
                                                    <h2>Careers</h2>
                                                  </div>
                                                  {/* <p>
                                            We’re always looking for talented people. Join
                                            our team!
                                          </p> */}
                                                </div>
                                              </div>
                                            </div>
                                          </div>
                                        </div>
                                      </div>
                                    </div>
                                  </div>
                                </div>
                              </div>
                            </div>
                            {/* <div>
                              <div
                                onClick={handleShowReport}
                                className={Styles.menu_item}
                                style={{ display: 'flex', justifyContent: 'start' }}
                              >
                                <div className={Styles.verticalMenuContainer}>
                                  <ReceptIcon className={Styles.navIcon} />
                                  Report
                                </div>
                              </div>
                            </div> */}
                            <div>
                              <div
                                onClick={handleShowReport}
                                className={Styles.menu_item}
                                style={{ display: 'flex', justifyContent: 'start' }}
                              >
                                <div className={Styles.verticalMenuContainer} onClick={() => {
                                  navigate('/reports');
                                }}>
                                  <ReceptIcon className={Styles.navIcon} />
                                  Report
                                </div>
                              </div>
                            </div>
                            <div>
                              <div
                                onClick={handleShowProject}
                                className={Styles.menu_item}
                                style={{ display: 'flex', justifyContent: 'left' }}
                              >
                                <div
                                  onClick={() => {
                                    navigate('/project-list');
                                  }}
                                  className={Styles.verticalMenuContainer}
                                >
                                  <BookIcon className={Styles.navIcon} />
                                  Project
                                </div>
                              </div>
                            </div>
                          </div>
                          <div>
                            <div className={Styles.acountContainerForVerticalNav} >
                              <div ref={menuRef} onClick={toggleMenu}>
                                <div id="demo" style={{
                                  maxHeight: !isMenuOpen ? '0' : 'none',
                                  overflow: 'hidden',
                                  transition: 'max-height 0.5s ease',
                                }}>
                                  <div className={Styles.dropDownContainer}>
                                    <div className={Styles.dropDownContent}>
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
                                  </div>
                                </div>
                                <div className={Styles.profileDetailHeadForVerticalNav}>
                                  <Avatar
                                    imageUrl={profile}
                                    firstName={userData?.first_name}
                                    lastName={userData?.last_name}
                                    size={40}
                                  />
                                  <div
                                    className={Styles.profileContents}
                                    style={{ color: 'black' }}
                                  >
                                    <span className={Styles.profileName}>
                                      {userData?.first_name + ' ' + userData?.last_name}
                                    </span>
                                    <span className={Styles.profileRole}>
                                      {userData?.user_roles[0]?.role_data?.role_name}
                                    </span>
                                  </div>
                                </div>
                              </div>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            )
              : (
                ''
              )
          }

        </nav>
      )}

    </div>
  );
};

export default Navbar;
