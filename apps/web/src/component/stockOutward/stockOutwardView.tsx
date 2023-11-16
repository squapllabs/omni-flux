import React, { useEffect, useState } from 'react';
import { useParams } from 'react-router-dom';
import CustomCard from '../ui/CustomCard';
import stockOutwardService from '../../service/stock-outward-service';
import Styles from '../../styles/stockOutWardView.module.scss';
import {  useLocation } from 'react-router-dom';
import { format } from 'date-fns';
import ProjectSubheader from '../project/projectSubheader';

const StockOutWardView = () => {
  const routeParams = useParams();
  const stockOutWardId = Number(routeParams?.id);
  const [stockOutwardData, setStockOutWardData] = useState();
  const [date, setDate] = useState();
  const location = useLocation();
  const projectId = location.state?.projectId;

  const fetchData = async () => {
    const stockOutWardData = await stockOutwardService.getOneStockOutWardId(
      stockOutWardId
    );
    setStockOutWardData(stockOutWardData?.data);

    const stockOutwardDate = stockOutWardData?.data?.stock_outward_date;
    if (stockOutwardDate) {
      const dateObject = new Date(stockOutwardDate);
      const formattedDate = format(dateObject, 'MMM dd, yyyy');
      setDate(formattedDate);
    }
  };

  useEffect(() => {
    fetchData();
  }, []);

  return (
    <div className={Styles.screen}>
      <ProjectSubheader
        description=""
        navigation={`/project-edit/${projectId}`}
        title='StockOutWard Information'
      />
      <div className={Styles.dividerStyle}></div>
      <div className={Styles.cardContent}>
        <CustomCard>
          <div className={Styles.mainContent}>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>StockOutWard ID</div>
              <div className={Styles.rightData}>
                {' '}
                {stockOutwardData?.outward_id
                  ? `${stockOutwardData?.outward_id}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Site Name</div>
              <div className={Styles.rightData}>
                {' '}
                {stockOutwardData?.site_data?.name
                  ? `${stockOutwardData?.site_data?.name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Site Engineer Name</div>
              <div className={Styles.rightData}>
                {''}
                {stockOutwardData?.site_engineer_data?.first_name +
                ' ' +
                stockOutwardData?.site_engineer_data?.last_name
                  ? `${
                      stockOutwardData?.site_engineer_data?.first_name +
                      ' ' +
                      stockOutwardData?.site_engineer_data?.last_name
                    }`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>StockOutWard Date</div>
              <div className={Styles.rightData}>
                {''}
                {date ? date : 'Not Provided'}
              </div>
            </div>
          </div>
        </CustomCard>
        <div className={Styles.dataRows} style={{paddingTop: '20px'}}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>S No</th>
                <th className={Styles.tableHeading}>Item</th>
                <th className={Styles.tableHeading}>OutWard Quantity</th>
                <th className={Styles.tableHeading}>UOM</th>
              </tr>
            </thead>
            <tbody>
              {stockOutwardData?.stock_outward_details?.length === 0 ? (
                <tr>
                  <td colSpan="5">No data found</td>
                </tr>
              ) : (
                stockOutwardData?.stock_outward_details?.map(
                  (item: any, index: any) => {
                    return (
                      <tr>
                        <td>{index + 1}</td>
                        <td>{item?.item_data?.item_name}</td>
                        <td>{item?.outward_quantity}</td>
                        <td>{item?.uom_data?.name}</td>
                      </tr>
                    );
                  }
                )
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
};

export default StockOutWardView;
