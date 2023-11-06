import React from 'react';
import { useState } from 'react';
import StackedChart from './charts/stackedChart';
import Styles from '../../styles/newStyles/reportModule/purchasereport.module.scss';
import LineChart from './charts/lineChart';
import HorizontalBarChart from './charts/horizontalBarChart';
import CloseIcon from '../menu/icons/closeIcon';

const PurchaseReport = () => {
  const data: any = [
    {
      poNo: 'PO001',
      supplierName: 'Jegan',
      amount: 2.5,
      deliveryDate: '12-12-2023',
    },
    {
      poNo: 'PO002',
      supplierName: 'Jegan',
      amount: 2.5,
      deliveryDate: '12-12-2023',
    },
    {
      poNo: 'PO003',
      supplierName: 'Jegan',
      amount: 2.5,
      deliveryDate: '12-12-2023',
    },
    {
      poNo: 'PO004',
      supplierName: 'Jegan',
      amount: 2.5,
      deliveryDate: '12-12-2023',
    },
  ];
  const [menuList, setMenuList] = useState<any>([
    { value: 'DMPO', label: ' Daily/Monthly Pending Orders' },
    { value: 'PP', label: 'Purchase Performance' },
    { value: 'AVRP', label: 'Approved vs Rejected POs' },
  ]);
  const [selectedMenu, setSelectedMenu] = useState<any>('DMPO');
  const handleMenuClear = () => {
    setSelectedMenu('');
  };
  const onMenuClick = (value: string) => {
    setSelectedMenu(value);
  };
  return (
    <div className={Styles.container}>
      <div className={Styles.sideMenu}>
        <div className={Styles.menuHeading}>
          <span>QUICK ACCESS MENU</span>
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
        {selectedMenu === 'DMPO' && (
          <div className={Styles?.stackChartContainer}>
            <span className={Styles?.chartitle}>
              Daily/Monthly Pending Orders (Goods only)
            </span>
            <StackedChart />
          </div>
        )}
        {selectedMenu === 'PP' && (
          <div className={Styles?.stackChartContainer}>
            <div className={Styles?.stackChartContainer}>
              <span className={Styles?.chartitle}>Purchase Performance</span>
              <LineChart />
            </div>
          </div>
        )}
        {selectedMenu === 'AVRP' && (
          <div className={Styles?.horizontalChart}>
            <div>
              <span className={Styles?.chartitle}>
                Approved vs Rejected POs
              </span>
              <HorizontalBarChart />
            </div>

            <div className={Styles.tableContainer}>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>#</th>
                    <th>PO Number</th>
                    <th>Supplier Name</th>
                    <th>Amount</th>
                    <th>Delivery Date</th>
                  </tr>
                </thead>
                <tbody>
                  {data?.map((poData: any, index: any) => {
                    return (
                      <tr>
                        <td>{index + 1}</td>
                        <td>{poData?.poNo}</td>
                        <td>{poData?.supplierName}</td>
                        <td>{poData?.amount}</td>
                        <td>{poData?.deliveryDate}</td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default PurchaseReport;
