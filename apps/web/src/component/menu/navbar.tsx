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
import { resetAuth, setToken } from '../../redux/reducer';
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
import {
  useGetNewNotificationByUserID,
  useGetNotificationforUser,
  useUpdateNotificationStatus,
} from '../../hooks/notification-hooks';
import { format } from 'date-fns';
import StarIcon from './icons/starIcon';
import notificationService from '../../service/notification-service';
import InvoiceIcon from './icons/invoiceIcon';
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
  const [showInvoice, setShowInvoice] = useState(false);
  const menuRef = useRef<HTMLDivElement | null>(null);
  const notificationRef = useRef<HTMLDivElement | null>(null);
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userData: any = encryptedData.userData;
  const roleName =
    encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
  const profile: any =
    encryptedData?.userData?.user_profiles?.profile_image_url;
  const handleShowAssets = () => {
    setShowAssets(!showAssets);
    setShowResources(false);
    setShowProject(false);
    setShowReport(false);
    setShowInvoice(false);
  };
  const handleShowHome = () => {
    setShowHome(!showHome);
    setShowAssets(false);
    setShowResources(false);
    setShowProject(false);
    setShowReport(false);
    setShowInvoice(false);
  };
  const handleShowExpense = () => {
    setShowResources(!showResources);
    setShowAssets(false);
    setShowProject(false);
    setShowReport(false);
    setShowInvoice(false);
  };
  const handleShowProject = () => {
    setShowProject(!showProject);
    setShowResources(false);
    setShowAssets(false);
    setShowReport(false);
    setShowInvoice(false);
  };
  const handleShowReport = () => {
    setShowProject(false);
    setShowResources(false);
    setShowAssets(false);
    setShowInvoice(false);
    setShowReport(!showReport);
  };

  const handleShowInvoice = () => {
    setShowProject(false);
    setShowResources(false);
    setShowAssets(false);
    setShowReport(false);
    setShowInvoice(!showInvoice);
  };
  const { mutate: updateNotification } = useUpdateNotificationStatus();

  const { data: newNotificationCount } = useGetNewNotificationByUserID(
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
    resetMenustore();
    navigate('/indent-view');
  };
  const handlePurchaseOrder = () => {
    resetMenustore();
    navigate('/purchase-order');
  };
  const handlePurchaseRequest = () => {
    resetMenustore();
    navigate('/approved-indent-list');
  };
  const handlePurchaseRequestList = () => {
    resetMenustore();
    navigate('/purchase-request-list-all');
  };
  const handleVendorList = () => {
    resetMenustore();
    navigate('/vendor-list');
  };
  const handleExpenseApproval = () => {
    resetMenustore();
    navigate('/site-expense-approve');
  };
  const handleExpenseRecall = () => {
    resetMenustore();
    navigate('/expense-recall');
  };

  const handleExpenseList = () => {
    resetMenustore();
    navigate('/expense-list');
  };
  const handleFinanceView = () => {
    resetMenustore();
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
  } = useGetNotificationforUser(data);

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
    resetMenustore();
    navigate('/settings');
  };

  const notificationFunction = () => {
    handleOpen();
    setTimeout(() => {
      upadteReadStatus();
    }, 2000);
  };
  const resetMenustore = () => {
    dispatch(setToken({ key: 'settingsMenuID', value: null }));
    dispatch(setToken({ key: 'projectMenuID', value: null }));
  };
  const upadteReadStatus = () => {
    const Object: any = {
      notification_to_user_id: userData?.user_id,
    };
    updateNotification(Object, {
      onSuccess: (data) => {
        if (data?.status === true) {
          refetch();
          setNewNotificationCount(null);
        }
      },
    });
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
                resetMenustore();
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
            {roleName === 'ADMIN' ||
            roleName === 'PROJECT MANAGER' ||
            roleName === 'PLANNING ENGINEER' ||
            roleName === 'PURCHASE MANAGER' ? (
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
                        {
                          // roleName === 'FINANCE MANAGER'
                          //   ? 'Invoice '
                          //   :
                          roleName === 'PLANNING ENGINEER'
                            ? 'Indent'
                            : 'Procurement'
                        }
                      </div>

                      <DropdownIcon color="white" className={Styles.navIcon} />
                    </div>
                  }
                >
                  <div className={Styles.container}>
                    <div className={Styles.dropDownContainer}>
                      <div>
                        <div className={Styles.dropDownContent}>
                          {roleName === 'PLANNING ENGINEER' ||
                          roleName === 'PROJECT MANAGER' ? (
                            <div
                              className={Styles.dropDownItems}
                              onClick={handleIndentapproval}
                            >
                              <div className={Styles.itemsTitle}>
                                <h2>Indent Approval</h2>
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
                                <h2>Approved Indent</h2>
                              </div>
                            </div>
                          ) : null}
                          {/* <div className={Styles.dashedLine}></div> */}
                          {roleName === 'PURCHASE MANAGER' ||
                          roleName === 'PROJECT MANAGER' ||
                          roleName === 'ADMIN' ? (
                            <div
                              className={Styles.dropDownItems}
                              onClick={handlePurchaseRequestList}
                            >
                              <div className={Styles.itemsTitle}>
                                <h2>Purchase Request</h2>
                              </div>
                            </div>
                          ) : null}
                          {/* <div className={Styles.dashedLine}></div> */}
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
                          {/* <div className={Styles.dashedLine}></div> */}
                          {roleName === 'PURCHASE MANAGER' ||
                          roleName === 'ADMIN' ? (
                            <div
                              className={Styles.dropDownItems}
                              onClick={handleVendorList}
                            >
                              <div className={Styles.itemsTitle}>
                                <h2>Vendors</h2>
                              </div>
                            </div>
                          ) : null}

                          {/* <div className={Styles.dashedLine}></div> */}
                        </div>
                      </div>
                    </div>
                    <div className={Styles.dropDownContainer}>
                      <div></div>
                    </div>
                  </div>
                </Dropdown>
              </div>
            ) : null}
            <div>
              <div
                onClick={handleShowExpense}
                className={
                  showResources
                    ? `${Styles.menu_item} ${Styles.selected}`
                    : `${Styles.menu_item}`
                }
              >
                <Dropdown
                  // label={
                  //   roleName === 'PROJECT MANAGER' || roleName === 'ADMIN' ? (
                  //     <div className={Styles.menuContainer}>
                  //       <BoxIcon color="white" />
                  //       Expenses <DropdownIcon color="white" />
                  //     </div>
                  //   ) : null
                  // }
                  label={
                    <div className={Styles.menuContainer}>
                      <BoxIcon color="white" />
                      Expenses <DropdownIcon color="white" />
                    </div>
                  }
                >
                  <div className={Styles.container}>
                    <div className={Styles.dropDownContainer}>
                      <div>
                        <div className={Styles.dropDownContent}>
                          <div
                            className={Styles.dropDownItems}
                            onClick={handleExpenseList}
                          >
                            <div className={Styles.itemsTitle}>
                              <h2>Expense-Claim</h2>
                            </div>
                          </div>
                          {roleName === 'PROJECT MANAGER' ||
                          roleName === 'ADMIN' ? (
                            <div
                              className={Styles.dropDownItems}
                              onClick={handleExpenseApproval}
                            >
                              <div className={Styles.itemsTitle}>
                                {/* <CheckIcon /> */}
                                <h2>Expense-Approval</h2>
                              </div>
                              {/* <p>Manage your expenses approval</p> */}
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
                        </div>
                      </div>
                    </div>
                  </div>
                </Dropdown>
              </div>
            </div>
            <div>
              <div
                onClick={handleShowInvoice}
                className={
                  showInvoice
                    ? `${Styles.menu_item} ${Styles.selected}`
                    : `${Styles.menu_item}`
                }
              >
                {roleName === 'ADMIN' || roleName === 'FINANCE MANAGER' ? (
                  <div
                    className={Styles.menuContainer}
                    onClick={() => {
                      resetMenustore();
                      navigate('/finance-view');
                    }}
                  >
                    <InvoiceIcon className={Styles.navIcon} color="white" />
                    Invoice
                  </div>
                ) : null}
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
                <div
                  className={Styles.menuContainer}
                  onClick={() => {
                    resetMenustore();
                    navigate('/reports');
                  }}
                >
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
                  resetMenustore();
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
    </div>
  );
};

export default Navbar;
