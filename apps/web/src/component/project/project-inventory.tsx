import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectInventory.module.scss';
import { useParams } from 'react-router-dom';
import { environment } from '../../environment/environment';
import { formatBudgetValue } from '../../helper/common-function';
import CustomLoader from '../ui/customLoader';
import projectInventoryService from '../../service/projectInventory-service';
import StoreIcon from '../menu/icons/newStoreIcon';

const ProjectInventory = () => {
  const routeParams = useParams();
  const projectId = Number(routeParams?.id);

  const [rowsPerPage, setRowsPerPage] = useState(50);
  const [currentPage, setCurrentPage] = useState(1);
  const [tableData, setTableData] = useState([]);
  const [dataLoading, setDataLoading] = useState(false);

  const inventoryData: any = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    project_id: projectId,
  };

  useEffect(() => {
    const getAllData = async () => {
      try {
        setDataLoading(true);
      } finally {
        const result = await projectInventoryService.inventoryDetailData(
          inventoryData
        );
        if (result.message === 'success') {
          setTableData(result.content);
          setDataLoading(false);
        }
      }
    };
    getAllData();
  }, []);

  const nullLableNameFromEnv = `${environment.NULLVALUE}`;

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.topHeading}>
          <div className={Styles.heading}>
            <div className={Styles.headingOne}>
              <div className={Styles.subHeading}>
                <StoreIcon />
                <h3>Inventory</h3>
              </div>
            </div>
          </div>
        </div>

        <div className={Styles.tableContainer}>
          <div>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th className={Styles.tableHeading}>S.No</th>
                  <th className={Styles.tableHeading}>Item Name </th>
                  <th className={Styles.tableHeading}>In Stock</th>
                  <th className={Styles.tableHeading}>Rate</th>
                </tr>
              </thead>
              <tbody>
                {tableData && tableData.length > 0 ? (
                  tableData.map((data: any, index: number) => (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>
                        {data?.item_data?.item_name || nullLableNameFromEnv}
                      </td>
                      <td>{data.available_quantity || nullLableNameFromEnv}</td>
                      <td>
                        {data?.item_data?.rate
                          ? formatBudgetValue(data?.item_data?.rate)
                          : nullLableNameFromEnv}
                      </td>
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data available</td>
                    <td></td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};
export default ProjectInventory;
