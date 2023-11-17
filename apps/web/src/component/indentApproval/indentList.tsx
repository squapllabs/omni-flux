import React, { useState, useEffect } from 'react';
import Styles from '../../styles/indentList.module.scss';
import Button from '../ui/Button';
import Select from '../ui/selectNew';
import {
  useGetAllIndentbyUserRole,
  useGetByUserRoleIndent,
} from '../../hooks/indent-approval-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import ViewIcon from '../menu/icons/newViewIcon';
import CustomLoader from '../ui/customLoader';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import { useNavigate } from 'react-router';
import { useGetBymasertDataTypeDrop } from '../../hooks/masertData-hook';
import PdfDownloadIcon from '../menu/icons/pdfDownloadIcon';
import ReportGenerator from '../reportGenerator/pdfReport/invoice';
import CustomPagination from '../menu/CustomPagination';
import CustomGroupButton from '../ui/CustomGroupButton';
import ProjectSubheader from '../project/projectSubheader';
import { environment } from '../../environment/environment';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';

const IndentList = () => {
  const navigate = useNavigate();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const [activeButton, setActiveButton] = useState<string | null>('Pending');
  const [selectedValueType, setSelectedValueType] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [filterValues, setFilterValues] = useState({
    search_by_code: '',
  });
  const userData: any = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'desc',
    status: 'AC',
    approver_status: activeButton,
    project_approver_id: userID,
    priority: selectedValueType,
    indent_request_code: filterValues?.search_by_code,
  };
  /* Function to get all indent data */
  const {
    data: getIndentData,
    isLoading: FilterLoading,
    refetch,
  } = useGetAllIndentbyUserRole(userData);

  const { data: getPriorityType = [], isLoading: dropLoading } =
    useGetBymasertDataTypeDrop('PRTYPE');

  const SampleOption: any = [
    { label: 'Low', value: 'Low' },
    { label: 'Medium', value: 'Medium' },
    { label: 'High', value: 'High' },
  ];
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Pending', value: 'Pending' },
    { label: 'Approved', value: 'Approved' },
    { label: 'Rejected', value: 'Rejected' },
  ]);
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
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
  const handleReportGenerator = () => {
    const data: any = {
      title: 'Indent Request',
      name: 'indent_request',
    };
    ReportGenerator(data);
  };
  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton, selectedValueType]);
  useEffect(() => {
    const handlefilter = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handlefilter);
  }, [filterValues]);
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  const nullLableNameFromEnv = `${environment.NULLVALUE}`;

  return (
    <div className={Styles.container}>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        <div className={Styles.box}>
          <ProjectSubheader
            description="Indent list based on Projects"
            navigation={`/home`}
            title={'Indent List'}
          />
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
                  placeholder="Select from priority"
                >
                  {SampleOption.map((option: any) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </Select>
              </div>
              <div>
                <Input
                  label="Indent Code"
                  width="260px"
                  prefixIcon={<SearchIcon />}
                  name="search_by_code"
                  value={filterValues.search_by_code}
                  onChange={(e) => {
                    setFilterValues({
                      ...filterValues,
                      search_by_code: e.target.value,
                    });
                    setCurrentPage(1);
                  }}
                  placeholder="Search"
                />
              </div>
            </div>
            <div>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
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
                  <th className={Styles.tableHeading}>Priority</th>
                  <th className={Styles.tableHeading}>
                    Expected Delivery Date
                  </th>
                  <th className={Styles.tableHeading}>Description</th>
                  <th className={Styles.tableHeading}>Total Cost</th>
                  <th className={Styles.tableHeading}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {getIndentData?.total_count === 0 ? (
                  <tr>
                    <td colSpan="8" style={{ textAlign: 'center' }}>
                      No data found
                    </td>
                  </tr>
                ) : (
                  ''
                )}
                {getIndentData?.content?.map((data: any, index: number) => {
                  return (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.indent_request_code}</td>
                      <td>{data?.project_data?.project_name}</td>
                      <td>
                        {data?.requester_user_data?.first_name +
                          ' ' +
                          data?.requester_user_data?.last_name}
                      </td>
                      <td>{data?.priority}</td>
                      <td>
                        {format(
                          new Date(data?.expected_delivery_date),
                          'MMM dd, yyyy'
                        )}
                      </td>
                      <td>
                        {data?.description === ''
                          ? nullLableNameFromEnv
                          : data?.description}
                      </td>
                      <td>
                        {formatBudgetValue(
                          data?.total_cost ? data?.total_cost : 0
                        )}
                      </td>
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

export default IndentList;
