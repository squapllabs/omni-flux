import React, { useState, useEffect } from 'react';
import Styles from '../../styles/purchaseList.module.scss';
import Button from '../ui/Button';
import { useGetByUserRoleIndent } from '../../hooks/indent-approval-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import CustomLoader from '../ui/customLoader';
import { format } from 'date-fns';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useNavigate } from 'react-router-dom';
import ReportGenerator from '../reportGenerator/pdfReport/invoice';
import CustomPagination from '../menu/CustomPagination';
import ProjectSubheader from '../project/projectSubheader';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import Select from '../ui/selectNew';
import CustomMenu from '../ui/NewCustomMenu';

const PurchaseList = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const navigate = useNavigate();
  const { data: getAllmasterDataForDrop = [] } = useGetAllProjectDrop();
  const [selectedValue, setSelectedValue] = useState('');
  const [priorityValue, setPriorityValue] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [filterValues, setFilterValues] = useState({
    search_by_code: '',
    priority: '',
  });
  /* Function to get all approved indent list */
  const {
    mutate: postDataForFilter,
    data: getIndentData,
    isLoading: FilterLoading,
  } = useGetByUserRoleIndent();
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
      request_type: 'Head Office',
    };
    postDataForFilter(userData);
    setSelectedValue('');
    setPriorityValue('');
    setFilterValues({
      search_by_code: '',
      priority: '',
    });
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
      indent_request_code: filterValues.search_by_code,
      request_type: 'Head Office',
    };
    postDataForFilter(userData);
  };
  /* Function to change page */
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
    navigate(`/indent-request-detail/${indentId}`, {
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
    setFilterValues({
      ...filterValues,
      [filterValues?.priority]: searchValue,
    });
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
          <ProjectSubheader
            navigation={'/home'}
            description="Approved Indent List based on Project"
            title="Approved Indent List"
          />
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <AutoCompleteSelect
                name="project_id"
                label="Select Project"
                defaultLabel="Select from options"
                placeholder="Select from options"
                value={selectedValue}
                mandatory={false}
                showclearicon={false}
                onChange={() => handleProjectDropdownChange}
                onSelect={(value) => {
                  setSelectedValue(value);
                  setIsResetDisabled(false);
                }}
                optionList={getAllmasterDataForDrop}
                onAddClick={function (e: string): void {
                  throw new Error('Function not implemented.');
                }}
                addLabel={''}
              />
              <Select
                width="180px"
                name="priority"
                label="Priority"
                defaultLabel="Select from options"
                placeholder="Select from options"
                value={filterValues?.priority}
                onChange={(e) => handleDropdownChange(e)}
              >
                {options?.map((item: any, index: any) => {
                  return (
                    <option key={index} value={item.value}>
                      {item.label}
                    </option>
                  );
                })}
              </Select>
              <Input
                width="260px"
                prefixIcon={<SearchIcon />}
                label="Indent Code"
                name="search_by_code"
                value={filterValues.search_by_code}
                onChange={(e) => {
                  setFilterValues({
                    ...filterValues,
                    search_by_code: e.target.value,
                  });
                  setCurrentPage(1);
                  setIsResetDisabled(false);
                }}
                placeholder="Search by Code"
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
          </div>
          <div className={Styles.dividerStyle}></div>
        </div>
        <div className={Styles.tableContainer}>
          <div>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th className={Styles.tableHeading}>#</th>
                  <th className={Styles.tableHeading}>Indent Code</th>
                  <th className={Styles.tableHeading}>Project Name</th>
                  <th className={Styles.tableHeading}>Raised By</th>
                  <th className={Styles.tableHeading}>
                    Expected Delivery Date
                  </th>
                  <th className={Styles.tableHeading}>Priority</th>
                  <th className={Styles.tableHeading}>Approved By</th>
                  <th className={Styles.tableHeading}>Approved Date</th>
                  <th className={Styles.tableHeading}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {getIndentData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                    <td></td>
                    <td></td>
                    <td></td>
                    <td></td>
                  </tr>
                ) : (
                  ''
                )}
                {getIndentData?.content?.map((data: any, index: number) => {
                  const isAvailable = data?.purchase_request?.length;
                  const actions = [
                    {
                      label: 'Vendor Selection',
                      onClick: () => {
                        handleView(data?.indent_request_id, data?.project_id);
                      },
                    },
                    {
                      label: 'Purchase Request',
                      onClick: () => {
                        navigate(
                          `/purchase-request-list/${data.indent_request_id}`
                        );
                      },
                      disabled: isAvailable === 0,
                    },
                  ];
                  return (
                    <tr key={data.indent_request_code}>
                      <td key={data.indent_request_code}>
                        {startingIndex + index}
                      </td>
                      <td>{data?.indent_request_code}</td>
                      <td>{data?.project_data?.project_name}</td>
                      <td>
                        {data?.requester_user_data?.first_name +
                          ' ' +
                          data?.requester_user_data?.last_name}
                      </td>
                      <td>
                        {format(
                          new Date(data?.expected_delivery_date),
                          'MMM dd, yyyy'
                        )}
                      </td>
                      <td
                        className={
                          data?.priority === 'HIGH' ? Styles.highPriority : ''
                        }
                      >
                        {data?.priority}
                      </td>
                      <td>
                        {data?.approver_user_data?.first_name +
                          ' ' +
                          data?.approver_user_data?.last_name}
                      </td>
                      <td>
                        {' '}
                        {format(new Date(data?.approved_date), 'MMM dd, yyyy')}
                      </td>
                      <td>
                        <CustomMenu
                          actions={actions}
                          name={'ApproveIndentList'}
                        />
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        </div>
        <div className={Styles.pagination}>
          <CustomPagination
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
