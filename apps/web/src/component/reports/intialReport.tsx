import React, { useEffect, useState } from 'react';
import Styles from '../../styles/newStyles/reportModule/intialreport.module.scss';
import ReceptIcon from '../menu/icons/recepitIcon';
import CloseIcon from '../menu/icons/closeIcon';
import ApproveSelectDialogBox from '../ui/ApproveSelectComponet';
import NewCustomPopupComponent from '../ui/newCustomPopupComponent';
import PurchaseRequestForm from './reportForm/purchaseRequestForm';
import CustomSnackbar from '../ui/customSnackBar';
import PurchaseRequestItemForm from './reportForm/purchaseRequestItemForm';
import RFQRegisterForm from './reportForm/rfqRegisterForm';
import RFQRegisterItemForm from './reportForm/rfqRegisterItemForm';
import RFQRegisterSupplierForm from './reportForm/rfqRegisterSupplierForm';
import InwardRegister from './reportForm/inwardRegister';
import ProjectInward from './reportForm/projectInward';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import FinanceFormAPS from './reportForm/financeFormAPS';
import FinanceFormAMS from './reportForm/financeFormAMS';
import { customSort } from './../../helper/common-function';
import FlagIcon from '../menu/icons/flagIcon';
import MenuIcon from '../menu/icons/menuIcon';

const IntialReport = () => {
  const [menuList, setMenuList] = useState<any>([
    { value: '', label: 'All' },
    { value: 'PUR', label: 'PURCHASE' },
    { value: 'FCE', label: 'FINANCE' },
    { value: 'ITY', label: 'INVENTORY' },
  ]);
  const [selectedMenu, setSelectedMenu] = useState<any>('');
  const [selectedMain, setSelectedMain] = useState<any>('');
  const { data: getAllProjectForDrop = [] } = useGetAllProjectDrop();
  const mainItems: any = [
    {
      title: 'Purchase Order Register',
      description:
        'Details of all the purchase order along with their delivery status',
      menuValue: 'PUR',
      mainValue: 'POR',
      sortOrder: 1,
    },
    {
      title: 'Purchase Order Register (Item-wise)',
      description:
        'Item-wise details of all purchase orders along with delivered quantity',
      menuValue: 'PUR',
      mainValue: 'PORIW',
      sortOrder: 1,
    },
    {
      title: 'RFQ Register',
      description:
        'Details of all request for quotations along with bidding status',
      menuValue: 'PUR',
      mainValue: 'RFQR',
      sortOrder: 1,
    },
    {
      title: 'RFQ Register (Item-wise)',
      description: 'Item-wise details of all request for quotations',
      menuValue: 'PUR',
      mainValue: 'RFQRIW',
      sortOrder: 1,
    },
    {
      title: 'RFQ Register (Supplier-wise)',
      description: 'Supplier-wise details of all request for quotations',
      menuValue: 'PUR',
      mainValue: 'RFQRSW',
      sortOrder: 1,
    },
    // {
    //   title: 'Indent Register (Item wise)',
    //   description: 'Item-wise details of all indents',
    //   menuValue: 'PUR',
    //   mainValue: 'IRIW',
    // },
    // {
    //   title: 'Inward Register',
    //   description:
    //     'Item-wise details of all the products received via inward document',
    //   menuValue: 'ITY',
    //   mainValue: 'ITYIR',
    //   sortOrder: 2,
    // },
    {
      title: 'Project Inward',
      description: 'Project wise comparison of stock quantity and value',
      menuValue: 'ITY',
      mainValue: 'ITYPI',
      sortOrder: 2,
    },
    {
      title: 'Accounts Project Summary',
      description: 'Details of project-wise recivable and payable',
      menuValue: 'FCE',
      mainValue: 'FCEAPS',
      sortOrder: 3,
    },
    // {
    //   title: 'Accounts Monthly Summary',
    //   description: 'Details for month-wise recivable and payable',
    //   menuValue: 'FCE',
    //   mainValue: 'FCEAMS',
    //   sortOrder: 3,
    // },
  ];

  const sortedItems = mainItems.sort(
    customSort(mainItems, 'sortOrder', 'desc')
  );

  const [mainList, setManiList] = useState<any>(sortedItems);
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openSideMenu, setOpenSideMenu] = useState(true);
  const [width, setWidth] = useState<any>(75);
  const handleMenuClear = () => {
    setSelectedMenu('');
    setOpenSideMenu(false);
    setWidth(100);
  };
  const onMenuClick = (value: string) => {
    setSelectedMenu(value);
  };
  const handleReport = (value: any) => {
    console.log('handleReportvalue', value);
    setSelectedMain(value?.mainValue);
    setOpen(true);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  useEffect(() => {
    if (selectedMenu != '') {
      const matchingObjects = mainItems?.filter(
        (obj: any) => obj?.menuValue === selectedMenu
      );
      console.log('matchingObjects', matchingObjects);
      setManiList(matchingObjects);
    } else {
      setManiList(mainItems);
    }
  }, [selectedMenu]);
  return (
    <div>
      <div className={Styles.container}>
        {openSideMenu && (
          <div className={Styles.sideMenu}>
            <div className={Styles.menuHeading}>
              <span>QUICK ACCESS MENU</span>
              {/* <CloseIcon
                onClick={() => {
                  handleMenuClear();
                }}
              /> */}
            </div>
            <div className={Styles.dividerLine}></div>
            <div className={Styles.side_sideMenu}>
              {menuList?.map((menu: any, index: any) => {
                return (
                  <ol key={index}>
                    <li
                      value={menu?.value}
                      className={
                        selectedMenu === menu?.value ? Styles.selected : ''
                      }
                      onClick={() => onMenuClick(menu?.value)}
                      style={{ textTransform: 'uppercase' }}
                    >
                      {menu?.label}
                    </li>
                  </ol>
                );
              })}
            </div>
          </div>
        )}

        <div className={Styles?.mainMenu} style={{ width: `${width}%` }}>
          {!openSideMenu && (
            <div
              onClick={() => {
                setOpenSideMenu(true);
              }}
            >
              <MenuIcon />
            </div>
          )}
          <div className={Styles.cardBox}>
            {mainList?.map((mainData: any, index: any) => {
              return (
                <div>
                  <div className={Styles?.cardContainer}>
                    <div>
                      <div className={Styles?.cardtitle}>
                        <span>{mainData?.title}</span>
                      </div>
                      <div className={Styles?.cardDescription}>
                        <span>{mainData?.description}</span>
                      </div>
                    </div>
                    <div className={Styles.reportButton}>
                      <div
                        className={Styles.reportCard}
                        onClick={() => handleReport(mainData)}
                      >
                        <ReceptIcon color="#7f56d9" />
                        <span>Generate Report</span>
                      </div>
                    </div>
                  </div>
                </div>
              );
            })}
          </div>
        </div>
        <NewCustomPopupComponent
          contentLine1={
            selectedMain === 'POR'
              ? 'Details of all the purchase order along with their delivery status'
              : selectedMain === 'PORIW'
              ? 'Item-wise details of all purchase orders along with delivered quantity'
              : selectedMain === 'RFQR'
              ? 'Details of all request for quotations along with bidding status'
              : selectedMain === 'RFQRIW'
              ? 'Item-wise details of all request for quotations'
              : selectedMain === 'RFQRSW'
              ? 'Supplier-wise details of all request for quotations'
              : selectedMain === 'ITYIR'
              ? 'Item-wise details of all the products received via inward document'
              : selectedMain === 'ITYPI'
              ? 'Project wise comparison of stock quantity and value'
              : selectedMain === 'FCEAPS'
              ? 'Details of project-wise recivable and payable'
              : selectedMain === 'FCEAMS'
              ? 'Details for month-wise recivable and payable'
              : ''
          }
          title={
            selectedMain === 'POR'
              ? 'Purchase Order Register'
              : selectedMain === 'PORIW'
              ? 'Purchase Order Register (Item-wise)'
              : selectedMain === 'RFQR'
              ? 'Request For Quotation Register'
              : selectedMain === 'RFQRIW'
              ? 'Request For Quotation Register(Item-wise)'
              : selectedMain === 'RFQRSW'
              ? 'Request For Quotation Register (Supplier-wise)'
              : selectedMain === 'ITYIR'
              ? 'Inward Register'
              : selectedMain === 'ITYPI'
              ? 'Project Inward'
              : selectedMain === 'FCEAPS'
              ? 'Accounts Project Summary'
              : selectedMain === 'FCEAMS'
              ? 'Accounts Monthly Summary'
              : ''
          }
          handleClose={() => {
            setOpen(false);
          }}
          open={open}
          content={
            <div>
              {selectedMain === 'POR' && (
                <PurchaseRequestForm
                  open={open}
                  setOpen={setOpen}
                  setMessage={setMessage}
                  setOpenSnack={setOpenSnack}
                  getAllProjectForDrop={getAllProjectForDrop}
                />
              )}
              {selectedMain === 'PORIW' && (
                <PurchaseRequestItemForm
                  open={open}
                  setOpen={setOpen}
                  setMessage={setMessage}
                  setOpenSnack={setOpenSnack}
                  getAllProjectForDrop={getAllProjectForDrop}
                />
              )}
              {selectedMain === 'RFQR' && (
                <RFQRegisterForm
                  open={open}
                  setOpen={setOpen}
                  setMessage={setMessage}
                  setOpenSnack={setOpenSnack}
                  getAllProjectForDrop={getAllProjectForDrop}
                />
              )}
              {selectedMain === 'RFQRIW' && (
                <RFQRegisterItemForm
                  open={open}
                  setOpen={setOpen}
                  setMessage={setMessage}
                  setOpenSnack={setOpenSnack}
                  getAllProjectForDrop={getAllProjectForDrop}
                />
              )}
              {selectedMain === 'RFQRSW' && (
                <RFQRegisterSupplierForm
                  open={open}
                  setOpen={setOpen}
                  setMessage={setMessage}
                  setOpenSnack={setOpenSnack}
                  getAllProjectForDrop={getAllProjectForDrop}
                />
              )}
              {selectedMain === 'ITYIR' && (
                <InwardRegister
                  open={open}
                  setOpen={setOpen}
                  setMessage={setMessage}
                  setOpenSnack={setOpenSnack}
                  getAllProjectForDrop={getAllProjectForDrop}
                />
              )}
              {selectedMain === 'ITYPI' && (
                <ProjectInward
                  open={open}
                  setOpen={setOpen}
                  setMessage={setMessage}
                  setOpenSnack={setOpenSnack}
                  getAllProjectForDrop={getAllProjectForDrop}
                />
              )}
              {selectedMain === 'FCEAPS' && (
                <FinanceFormAPS
                  open={open}
                  setOpen={setOpen}
                  setMessage={setMessage}
                  setOpenSnack={setOpenSnack}
                  getAllProjectForDrop={getAllProjectForDrop}
                />
              )}
              {selectedMain === 'FCEAMS' && (
                <FinanceFormAMS
                  open={open}
                  setOpen={setOpen}
                  setMessage={setMessage}
                  setOpenSnack={setOpenSnack}
                  getAllProjectForDrop={getAllProjectForDrop}
                />
              )}
            </div>
          }
        />
      </div>
      <CustomSnackbar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default IntialReport;
