import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectInventory.module.scss';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import { environment } from '../../environment/environment';
import { formatBudgetValue } from '../../helper/common-function';
import CustomLoader from '../ui/customLoader';
import projectInventoryService from '../../service/projectInventory-service';
import ProjectSubheader from '../project/projectSubheader';


const ProjectInventory = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const projectId = Number(routeParams?.id);

  const [rowsPerPage, setRowsPerPage] = useState(50);
  const [currentPage, setCurrentPage] = useState(1);
  const [tableData, setTableData] = useState([]);
  const [dataLoading, setDataLoading] = useState(false);

  const inventoryData = {
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
      <ProjectSubheader
          description='Manage your Indent raise detail across your project'
          // navigation={`/project-edit/${projectId}`}
          navigation={`/project-list`}
          title='Project Inventory Detail List'
        />
        <div className={Styles.dividerStyle}></div>
        
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
