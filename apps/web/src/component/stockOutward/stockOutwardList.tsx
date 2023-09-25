import React, { useState, useEffect } from 'react';
import Styles from '../../styles/stockOutwardList.module.scss';
import Button from '../ui/Button';
import { getAllStockOutwardData } from '../../hooks/stock-outward';
import Pagination from '../menu/pagination';
import CustomLoader from '../ui/customLoader';
import { format } from 'date-fns';
import { useNavigate } from 'react-router';
import AddIcon from '../menu/icons/addIcon';
import { useGetAllProject } from '../../hooks/project-hooks';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import ViewIcon from '../menu/icons/viewIcon';
import EditIcon from '../menu/icons/editIcon';
import { useParams } from 'react-router-dom';


const StockOutwardList = () => {
  const navigate = useNavigate();
  const routeParams = useParams();

  const [selectedValue, setSelectedValue] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const {
    mutate: postDataForFilter,
    data: getStockData,
    isLoading: FilterLoading,
  } = getAllStockOutwardData();
  const { data: getAllProjectDataForDrop = [], isLoading: dropLoading } =
    useGetAllProject();


  const handleReset = async () => {
    setIsResetDisabled(true);
    setSelectedValue('');
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(userData);
  };
  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const stockData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
      site_id: "",
      project_id: selectedValue
    };
    postDataForFilter(stockData);
  };

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedProjectId = event.target.value;
    setSelectedValue(selectedProjectId);
    setIsResetDisabled(searchValue === '');
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage]);
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div className={Styles.container}>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Stock Outward</h3>
            <span className={Styles.content}>
              Stock Outward manage your entire organization.
            </span>
          </div>
          <div className={Styles.dividerStyleTop}></div>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <div>
                <AutoCompleteSelect
                  name="parent_master_data_id"
                  defaultLabel="Select Project Name"
                  onChange={() => handleDropdownChange}
                  value={selectedValue}
                  placeholder="Select Project Name"
                  width="260px"
                  onSelect={(value) => {
                    setSelectedValue(value);
                    setIsResetDisabled(false);
                  }}
                  optionList={
                    dropLoading === true ? [] : getAllProjectDataForDrop
                  }
                />
              </div>
              <Button
                className={Styles.searchButton}
                shape="rectangle"
                justify="center"
                size="small"
                onClick={handleSearch}
              >
                Search
              </Button>
              <Button
                className={Styles.resetButton}
                shape="rectangle"
                justify="center"
                size="small"
                disabled={isResetDisabled}
                onClick={handleReset}
              >
                Reset
              </Button>
            </div>
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon color='white' />}
                onClick={() => navigate('/stockoutward-add', { state: { projectId: routeParams?.id } })}
              >
                Add
              </Button>
            </div>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Project</th>
                  <th>Outward ID</th>
                  <th>Site</th>
                  <th>Site Engineer</th>
                  <th>Item Count</th>
                  <th>Delivery Date</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {getStockData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                  </tr>
                ) : (
                  ''
                )}
                {getStockData?.content?.map((data: any, index: number) => {
                  return (
                    <tr key={data.stock_outward_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.project_data?.project_name}</td>
                      <td>{data?.outward_id}</td>
                      <td>{data?.site_data?.name}</td>
                      <td>
                        {data?.site_engineer_data?.first_name +
                          ' ' +
                          data?.site_engineer_data?.last_name}
                      </td>
                      <td>{data?.stock_outward_details?.length}</td>
                      <td>
                        {format(
                          new Date(data?.stock_outward_date),
                          'dd-MMM-yyyy'
                        )}
                      </td>
                      <td>
                        <div className={Styles.tablerow}>
                          <EditIcon onClick={() => navigate(`/stockoutward-edit/${data?.stock_outward_id}`, { state: { projectId: routeParams?.id } })} />
                          <ViewIcon onClick={() => navigate(`/stockoutward-view/${data?.stock_outward_id}`,{ state: { projectId: routeParams?.id } })} />
                        </div>
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        </div>
        <div className={Styles.pagination}>
          <Pagination
            currentPage={currentPage}
            totalPages={getStockData?.total_page}
            totalCount={getStockData?.total_count}
            rowsPerPage={rowsPerPage}
            onPageChange={handlePageChange}
            onRowsPerPageChange={handleRowsPerPageChange}
          />
        </div>
      </CustomLoader>
    </div>
  );
};

export default StockOutwardList;
