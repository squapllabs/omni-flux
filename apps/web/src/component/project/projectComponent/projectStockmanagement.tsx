import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/project.module.scss';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import { useNavigate, useParams } from 'react-router-dom';
import {
  useGetAllStockAudits,
  getByFilterStockAudit,
} from '../../../hooks/stockAudit-hooks';
import CustomLoader from '../../ui/customLoader';
import { format } from 'date-fns';

const ProjectStockmanagement = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const {
    mutate: postDataForFilter,
    data: getStockAuditList,
    isLoading: fetchLoader,
  } = getByFilterStockAudit();
  console.log('getStockAuditList', getStockAuditList?.content);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage]);

  /* Function for search */
  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      project_id: Number(routeParams?.id),
      site_id: '',
    };
    postDataForFilter(demo);
  };

  /* Function for resting the search field and data to normal state */
  const handleReset = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(demo);
  };
  return (
    <div>
      <CustomLoader loading={fetchLoader}>
        <div className={Styles.buttons}>
          <Button
            type="button"
            color="primary"
            shape="rectangle"
            size="small"
            justify="center"
            icon={<AddIcon width={20} color="white" />}
            onClick={() => {
              navigate(`/project-stockadd/${routeParams?.id}`);
            }}
          >
            Add
          </Button>
        </div>
        <div className={Styles.tableContainer}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>S.No</th>
                <th>Site</th>
                <th>Audit Date</th>
                <th>Item count</th>
                <th>Action</th>
              </tr>
            </thead>
            <tbody>
              {getStockAuditList?.content?.map((items: any, index: any) => {
                rowIndex = rowIndex + 1;
                return (
                  <tr>
                    <td>{rowIndex}</td>
                    <td>{items?.site_data?.name}</td>
                    <td>{dateFormat(items?.stock_audit_date)}</td>
                    <td>{items?.item_details?.length}</td>
                    <td></td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </CustomLoader>
    </div>
  );
};

export default ProjectStockmanagement;
