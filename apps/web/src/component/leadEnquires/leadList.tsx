import React, { useState, useEffect } from 'react';
import Styles from '../../styles/leadTender.module.scss';
import { getBySearchLeadEnquiry } from '../../hooks/leadEnquires-hooks';
import EditIcon from '../menu/icons/editIcon';
import Pagination from '../menu/pagination';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import CustomGroupButton from '../ui/CustomGroupButton';
import SearchIcon from '../menu/icons/search';
import Input from '../ui/Input';
import CustomLoader from '../ui/customLoader';
import { useNavigate } from 'react-router-dom';
import ViewIcon from '../menu/icons/viewIcon';

const LeadList = () => {
  const navigate = useNavigate();
  const {
    mutate: postleadEnquiry,
    data: filterData,
    isLoading: getFilterLoading,
  } = getBySearchLeadEnquiry();
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(5);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [disable, setDisable] = useState(true);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'active', value: 'AC' },
    { label: 'inactive', value: 'IC' },
  ]);
  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);
  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
    };
    postleadEnquiry(demo);
    setIsLoading(false);
    setFilter(true);
    setDisable(false);
  };
  const handleReset = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
    };
    postleadEnquiry(demo);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
    setDisable(false);
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
  const handleAdd = () => {
    navigate('/lead-add');
  };
  const handleEdit = (id: any, name: any) => {
    navigate(`/lead-edit/${id}/${name}`);
  };
  const handleView = (id: any, name: any) => {
    if (name === 'Tender') navigate(`/lead-info-tender/${id}`);
    else navigate(`/lead-info-product/${id}`);
  };
  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
  };
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  return (
    <div>
      <CustomLoader loading={getFilterLoading} size={48} color="#333C44">
        <div className={Styles.container}>
          <div className={Styles.box}>
            <div className={Styles.addContainer}>
              <div className={Styles.textContent}>
                <h3>Add - Lead/Enquiry</h3>
                <span className={Styles.content}>
                  Add your Lead & Enquiries
                </span>
              </div>
              <div className={Styles.addButton}>
                <Button
                  shape="rectangle"
                  justify="center"
                  size="small"
                  color="primary"
                  icon={<AddIcon />}
                  onClick={handleAdd}
                >
                  Lead
                </Button>
              </div>
            </div>
          </div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List - Lead/Enquiry</h3>
              <span className={Styles.content}>
                List of your Lead & Enquiries
              </span>
            </div>
            <div className={Styles.searchField}>
              <div className={Styles.inputFilter}>
                <Input
                  width="260px"
                  prefixIcon={<SearchIcon />}
                  name="search_by_name"
                  value={filterValues.search_by_name}
                  onChange={(e) => handleFilterChange(e)}
                  placeholder="Search by item name"
                />
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
                  disabled={disable}
                  onClick={handleReset}
                >
                  Reset
                </Button>
              </div>

              <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div>
            </div>
            <div className={Styles.tableContainer}>
              <table>
                <thead>
                  <tr>
                    <th>S No</th>
                    <th>Lead Type</th>
                    <th>Lead Code</th>
                    <th>Client Name</th>
                    <th>Client Level</th>
                    <th></th>
                  </tr>
                </thead>
                <tbody>
                  {filterData?.total_count === 0 ? (
                    <tr>
                      <td></td>
                      <td>No data found</td>
                      <td></td>
                    </tr>
                  ) : (
                    ''
                  )}
                  {filterData?.content?.map((item: any, index: number) => (
                    <tr>
                      <td>{index + 1}</td>
                      <td>{item.lead_type}</td>
                      <td>{item.lead_code}</td>
                      <td>{item.client_contact_name}</td>
                      <td>{item.client_level_info?.master_data_name}</td>
                      <td>
                        <div className={Styles.tablerow}>
                          <EditIcon
                            onClick={() =>
                              handleEdit(item.lead_enquiry_id, item.lead_type)
                            }
                          />
                          <ViewIcon
                            onClick={() =>
                              // navigate(`/lead-info-product/${item.lead_enquiry_id}`)
                              handleView(item.lead_enquiry_id, item.lead_type)
                            }
                          />
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
            <div className={Styles.pagination}>
              <Pagination
                currentPage={currentPage}
                totalPages={filterData?.total_page}
                rowsPerPage={rowsPerPage}
                onPageChange={handlePageChange}
                onRowsPerPageChange={handleRowsPerPageChange}
              />
            </div>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default LeadList;
