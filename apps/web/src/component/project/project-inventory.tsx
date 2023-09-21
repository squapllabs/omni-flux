import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectInventory.module.scss';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import { environment } from '../../environment/environment';
import { formatBudgetValue } from '../../helper/common-function';
import CustomLoader from '../ui/customLoader';
import projectInventoryService from '../../service/projectInventory-service';

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
        <div className={Styles.textContent}>
          <h3>Project Inventory Detail List</h3>
          <span className={Styles.content}>
            Manage your Indent raise detail across your project
          </span>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S.No</th>
                  <th>Item Name </th>
                  <th>In Stock</th>
                  <th>Rate</th>
                </tr>
              </thead>
              <tbody>
                {tableData?.map((data: any, index: number) => (
                  <tr key={data.indent_request_id}>
                    <td>{startingIndex + index}</td>
                    <td>
                      {data?.item_data?.item_name || nullLableNameFromEnv}
                    </td>
                    <td>{data.available_quantity || nullLableNameFromEnv}</td>
                    <td>
                      { data?.item_data?.rate ? formatBudgetValue(data?.item_data?.rate) :
                        nullLableNameFromEnv}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          <div className={Styles.Buttons}>
            <div>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="secondary"
                onClick={() => navigate('/project-list')}
              >
                Back
              </Button>
            </div>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};
export default ProjectInventory;