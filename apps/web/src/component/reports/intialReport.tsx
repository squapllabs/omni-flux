import React, { useEffect, useState } from 'react';
import Styles from '../../styles/newStyles/reportModule/intialreport.module.scss';
import ReceptIcon from '../menu/icons/recepitIcon';
import CloseIcon from '../menu/icons/closeIcon';
import ApproveSelectDialogBox from '../ui/ApproveSelectComponet';
import NewCustomPopupComponent from '../ui/newCustomPopupComponent';
import PurchaseRequestForm from './reportForm/purchaseRequestForm';
import CustomSnackbar from '../ui/customSnackBar';

const IntialReport = () => {
  const [menuList, setMenuList] = useState<any>([
    { value: '', label: 'All' },
    { value: 'PUR', label: 'PURCHASE' },
    { value: 'FCE', label: 'FINANCE' },
    { value: 'ITY', label: 'INVENTORY' },
  ]);
  const [selectedMenu, setSelectedMenu] = useState<any>('');
  const [selectedMain, setSelectedMain] = useState<any>('');

  const mainItems: any = [
    {
      title: 'Purchase Order Register',
      description:
        'Details of all the purchase order along with their delivery status',
      menuValue: 'PUR',
      mainValue: 'POR',
    },
    {
      title: 'Purchase Order Register (Item-wise)',
      description:
        'Item-wise details of all purchase orders along with delivered quantity',
      menuValue: 'PUR',
      mainValue: 'PORIW',
    },
    {
      title: 'RFQ Register',
      description:
        'Details of all request for quotations along with bidding status',
      menuValue: 'PUR',
      mainValue: 'RFQR',
    },
    {
      title: 'RFQ Register (Item-wise)',
      description: 'Item-wise details of all request for quotations',
      menuValue: 'PUR',
      mainValue: 'RFQRIW',
    },
    {
      title: 'RFQ Register (Supplier-wise)',
      description: 'Supplier-wise details of all request for quotations',
      menuValue: 'PUR',
      mainValue: 'PORIW',
    },
    {
      title: 'Indent Register (Item wise)',
      description: 'Item-wise details of all indents',
      menuValue: 'PUR',
      mainValue: 'IRIW',
    },
    {
      title: 'Inward Register',
      description:
        'Item-wise details of all the products received via inward document',
      menuValue: 'ITY',
      mainValue: 'ITYIR',
    },
    {
      title: 'GRN/Inward Invoice',
      description:
        'Item-wise comparison of PO, Inward, GRN and Invoice quantity',
      menuValue: 'ITY',
      mainValue: 'ITYGRN',
    },
  ];
  const [mainList, setManiList] = useState<any>(mainItems);
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const handleMenuClear = () => {
    setSelectedMenu('');
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
        <div className={Styles.sideMenu}>
          <div className={Styles.menuHeading}>
            <span>QUICK ACCESS MENU</span>
            <CloseIcon
              onClick={() => {
                handleMenuClear();
              }}
            />
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
        <div className={Styles?.mainMenu}>
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
                        <span>Genderate Report</span>
                      </div>
                    </div>
                  </div>
                </div>
              );
            })}
          </div>
        </div>
        <NewCustomPopupComponent
          contentLine1="Report Generation"
          title={selectedMain === 'POR' ? 'Purchase Order Register' : ''}
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
