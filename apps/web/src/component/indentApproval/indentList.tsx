import React, { useState, useEffect } from 'react';
import Styles from '../../styles/indentList.module.scss';
import Button from '../ui/Button';
import Select from '../ui/selectNew';
import { getByUserRoleIndent } from '../../hooks/indent-approval-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import Pagination from '../menu/pagination';
import ViewIcon from '../menu/icons/viewIcon';
import CustomLoader from '../ui/customLoader';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import { useNavigate } from 'react-router';
import { getBymasertDataTypeDrop } from '../../hooks/masertData-hook';

const IndentList = () => {
  const navigate = useNavigate();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const [selectedValueType, setSelectedValueType] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const {
    mutate: postDataForFilter,
    data: getIndentData,
    isLoading: FilterLoading,
  } = getByUserRoleIndent();
  const { data: getPriorityType = [], isLoading: dropLoading } = getBymasertDataTypeDrop('PRTYPE');

  const SampleOption: any = [
    { label: 'Low', value: 'Low' },
    { label: 'Medium', value: 'Medium' },
    { label: 'High', value: 'High' },
  ];

  const handleReset = async () => {
    setIsResetDisabled(true);
    setSelectedValueType('');
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      approver_status: 'Pending',
      project_approver_id: userID,
      priority: '',
    };
    postDataForFilter(userData);
  };
  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      approver_status: 'Pending',
      project_approver_id: userID,
      priority: selectedValueType,
    };
    postDataForFilter(userData);
  };

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
  const handleDropdownChangePriorityType = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedData = event.target.value;
    setSelectedValueType(selectedData);
    setIsResetDisabled(searchValue === '');
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
            <h3>Indent List</h3>
            <span className={Styles.content}>
              Indent list based on projects.
            </span>
          </div>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <div>
                <Select
                  width="200px"
                  label="Project Type"
                  name="project_type"
                  onChange={handleDropdownChangePriorityType}
                  value={selectedValueType}
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                >
                  {SampleOption.map((option: any) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </Select>
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
          </div>
          <div className={Styles.dividerStyle}></div>
        </div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Project Name</th>
                  <th>Priority</th>
                  <th>Expected Delivery Date</th>
                  <th>Des</th>
                  <th>Total Cost</th>
                  <th>Options</th>
                </tr>
              </thead>
              <tbody>
                {getIndentData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                  </tr>
                ) : (
                  ''
                )}
                {getIndentData?.content?.map((data: any, index: number) => {
                  return (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.project_data?.project_name}</td>
                      <td>{data?.priority}</td>
                      <td>
                        {format(
                          new Date(data?.expected_delivery_date),
                          'MMM dd, yyyy'
                        )}
                      </td>
                      <td>{data?.description}</td>
                      <td>{formatBudgetValue(data?.total_cost)}</td>
                      <td>
                        <div className={Styles.tablerow}>
                          <ViewIcon
                            onClick={() =>
                              navigate(
                                `/indent-detail/${data?.indent_request_id}`
                              )
                            }
                          />
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
            totalPages={getIndentData?.total_page}
            totalCount={getIndentData?.total_count}
            rowsPerPage={rowsPerPage}
            onPageChange={handlePageChange}
            onRowsPerPageChange={handleRowsPerPageChange}
          />
        </div>
      </CustomLoader>
    </div>
  );
};

export default IndentList;
