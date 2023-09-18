import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import CustomSnackBar from '../ui/customSnackBar';
import BackArrowIcon from '../menu/icons/backArrow';
import { useGetOnePurchaseRequest } from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import CustomLoader from '../ui/customLoader';

const IndentView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const IndentId = Number(routeParams?.id);
  const {
    data: getAllData,
    isLoading: dataLoading,
    refetch,
  } = useGetOnePurchaseRequest(IndentId);
  console.log('llllllllll', getAllData);
  useEffect(() => {
    refetch();
  }, []);

  //   const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.headingTop}>
          <div className={Styles.textContent}>
            <h3>Project Name</h3>
            <span className={Styles.content}>Project description</span>
          </div>
          <div className={Styles.rightContent}>
            <h3>Estimated Delivery Date : </h3>
            <span> 20-10-2023</span>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Vendor Name </th>
                  <th>Item</th>
                  <th>Quantity</th>
                </tr>
              </thead>
              {/* <tbody>
                {getAllData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                  </tr>
                ) : (
                  ''
                )}
                {getAllData?.content?.map((data: any, index: number) => {
                  return (
                    <tr key={data.indent_request_id}>
                      <td>{index + 1}</td>
                      <td>{data?.bom_detail_data?.item_data?.item_name}</td>
                      <td>{data?.bom_detail_data?.uom_data?.name}</td>
                      <td>{data?.quantity}</td>
                      <td>{formatBudgetValue(data?.total)}</td>
                    </tr>
                  );
                })}
              </tbody> */}
            </table>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default IndentView;
