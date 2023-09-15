import React, { useState, useEffect } from 'react';
import Styles from '../../styles/indentList.module.scss';
import Input from '../ui/Input';
import Button from '../ui/Button';
import SearchIcon from '../menu/icons/search';
import { getByUserRoleIndent } from '../../hooks/indent-approval-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import Pagination from '../menu/pagination';
// import EditIcon from '../menu/icons/editIcon';
import ViewIcon from '../menu/icons/viewIcon';
import CustomLoader from '../ui/customLoader';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Select from '../ui/selectNew';

const PurchaseList = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const { data: getAllmasterDataForDrop = [], isLoading: dropLoading } =
    useGetAllProjectDrop();
  //   const [buttonLabels, setButtonLabels] = useState([
  //     { label: 'Pending', value: 'Pending' },
  //     { label: 'Approved', value: 'Approved' },
  //     { label: 'Rejected', value: 'Rejected' },
  //   ]);
  //   const [activeButton, setActiveButton] = useState<string | null>('Pending');
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [selectedValue, setSelectedValue] = useState('');
  const [priorityValue, setPriorityValue] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const {
    mutate: postDataForFilter,
    data: getIndentData,
    isLoading: FilterLoading,
  } = getByUserRoleIndent();
  console.log('dta====>', getIndentData);

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
    setFilterValues({
      search_by_name: '',
    });
    setSelectedValue('');
    setIsResetDisabled(true);
  };
  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      global_search: filterValues.search_by_name,
      project_id: Number(selectedValue),
      priority: priorityValue,
      status: 'AC',
      approver_status: 'Approved',
    };
    postDataForFilter(userData);
    console.log("userData",userData);
    
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
            <h3>Indent List</h3>
            <span className={Styles.content}>Indent List based on Project</span>
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
                  <th>Description </th>
                  <th>Requester</th>
                  <th>Expected Delivery Date</th>
                  <th>Total Cost</th>
                  <th></th>
                </tr>
              </thead>
              <tbody>
                {getIndentData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                    <td></td>
                    {/* {activeButton === 'Pending' && <td></td>} */}
                  </tr>
                ) : (
                  ''
                )}
                {getIndentData?.content?.map((data: any, index: number) => {
                  return (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.project_data?.project_name}</td>
                      <td>{data?.description}</td>
                      <td>
                        {data?.requester_user_data?.first_name || ''}{' '}
                        {data?.requester_user_data?.last_name
                          ? data?.requester_user_data?.last_name
                          : ''}
                      </td>
                      <td>
                        {format(
                          new Date(data?.expected_delivery_date),
                          'MMM dd, yyyy'
                        )}
                      </td>
                      <td>{formatBudgetValue(data?.total_cost)}</td>
                      <td>
                        <div className={Styles.tablerow}>
                          <ViewIcon
                            onClick={
                              () => alert('view')
                              //   navigate(
                              //     `/project-info/${data?.project_data.project_id}`
                              //   )
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
