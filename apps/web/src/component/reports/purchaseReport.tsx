import React from 'react';
import { useState } from 'react';
import StackedChart from './charts/stackedChart';
import Styles from '../../styles/newStyles/reportModule/purchasereport.module.scss';
import LineChart from './charts/lineChart';
import HorizontalBarChart from './charts/horizontalBarChart';

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
  return (
    <div className={Styles.container}>
      <div className={Styles?.stackChartContainer}>
        <span className={Styles?.chartitle}>
          Daily/Monthly Pending Orders (Goods only)
        </span>
        <StackedChart />
      </div>
      <div className={Styles.dividerLine}></div>
      <div className={Styles?.stackChartContainer}>
        <span className={Styles?.chartitle}>Purchase Performance</span>
        <LineChart />
      </div>
      <div className={Styles.dividerLine}></div>
      <div className={Styles?.horizontalChart}>
        <div>
          <span className={Styles?.chartitle}>Approved vs Rejected POs</span>
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
    </div>
  );
};

export default PurchaseReport;
