import React, { useState, useEffect } from 'react';
import Styles from '../../styles/purchaseList.module.scss';
import Button from '../ui/Button';
import { getByUserRoleIndent } from '../../hooks/indent-approval-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import Pagination from '../menu/pagination';
import ViewIcon from '../menu/icons/viewIcon';
import CustomLoader from '../ui/customLoader';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useNavigate } from 'react-router-dom';

const PurchaseList = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const navigate = useNavigate();
  const { data: getAllmasterDataForDrop = [] } = useGetAllProjectDrop();
  //   const [buttonLabels, setButtonLabels] = useState([
  //     { label: 'Pending', value: 'Pending' },
  //     { label: 'Approved', value: 'Approved' },
  //     { label: 'Rejected', value: 'Rejected' },
  //   ]);
  const [selectedValue, setSelectedValue] = useState('');
  const [priorityValue, setPriorityValue] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [projectId, setProjectId] = useState();
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const {
    mutate: postDataForFilter,
    data: getIndentData,
    isLoading: FilterLoading,
  } = getByUserRoleIndent();

  const handleReset = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      project_id: '',
      priority: '',
      status: 'AC',
      approver_status: 'Approved',
    };
    postDataForFilter(userData);
    setSelectedValue('');
    setPriorityValue('');
    setIsResetDisabled(true);
  };
  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      project_id: Number(selectedValue),
      priority: priorityValue,
      status: 'AC',
      approver_status: 'Approved',
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

  const handleView = (indentId: any, projectId: any) => {
    navigate(`/purchase-detail/${indentId}`, {
      state: { project_id: projectId },
    });
  };

  const options: any = [
    { value: 'Low', label: 'Low' },
    { value: 'Medium', label: 'Medium' },
    { value: 'High', label: 'High' },
  ];

  const handleProjectDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedProjectId = event.target.value;
    setSelectedValue(selectedProjectId);
    setIsResetDisabled(searchValue === '');
  };
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedPriority = event.target.value;
    setPriorityValue(selectedPriority);
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
            <h3>Purchase List</h3>
            <span className={Styles.content}>Purchase List based on Project</span>
          </div>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <AutoCompleteSelect
                name="project_id"
                label="Select Project"
                defaultLabel="Select from options"
                placeholder="Select from options"
                value={selectedValue}
                onChange={() => handleProjectDropdownChange}
                onSelect={(value) => {
                  setSelectedValue(value);
                  setIsResetDisabled(false);
                }}
                optionList={getAllmasterDataForDrop}
              />
              <AutoCompleteSelect
                name="priority"
                label="Priority"
                defaultLabel="Select from options"
                placeholder="Select from options"
                value={selectedValue}
                onChange={() => handleDropdownChange}
                onSelect={(value) => {
                  setPriorityValue(value);
                  setIsResetDisabled(false);
                }}
                optionList={options}
              />
              <div className={Styles.buttonStyle}>
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
            {/* <div>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
            </div> */}
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
                  <th>Expected Delivery Date </th>
                  <th>Priority</th>
                  <th>Cost</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {getIndentData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                    <td></td>
                  </tr>
                ) : (
                  ''
                )}
                {getIndentData?.content?.map((data: any, index: number) => {
                  return (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.project_data?.project_name}</td>
                      <td>
                        {format(
                          new Date(data?.expected_delivery_date),
                          'MMM dd, yyyy'
                        )}
                      </td>
                      {/* <td>{data?.priority}</td> */}
                      <td
                        className={
                          data?.priority === 'HIGH' ? Styles.highPriority : ''
                        }
                      >
                        {data?.priority}
                      </td>
                      <td>{formatBudgetValue(data?.total_cost)}</td>
                      <td>
                        <div className={Styles.tablerow}>
                          <ViewIcon
                            onClick={() =>
                              handleView(
                                data?.indent_request_id,
                                data?.project_id
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

export default PurchaseList;
