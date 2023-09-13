import React, { useState, useEffect } from 'react';
import Button from '../menu/button';
import { useNavigate } from 'react-router-dom';
import Styles from '..//../styles/listItem.module.scss';
import Pagination from '../menu/pagination';
import AddIcon from '../menu/icons/addIcon';
import DownloadIcon from '../menu/icons/download';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import ButtonOne from '../ui/Button';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomLoader from '../ui/customLoader';
import { getByItem } from '../../hooks/add-product-hooks';
import EditIcon from '../menu/icons/editIcon';
import { formatBudgetValue } from '../../helper/common-function';
import addProduct from '../../service/add-product';

const ProductPage = () => {
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: searchLoader,
  } = getByItem();

  const navigate = useNavigate();
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [isResetDisabled, setIsResetDisabled] = useState(true);

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
    setIsResetDisabled(searchValue === '');
    if (searchValue === '') {
      handleReset();
    }
  };

  const handleSearch = async () => {
    const itemData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
    };
    postDataForFilter(itemData);
  };

  const handleReset = async () => {
    const itemData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(itemData);
    setFilterValues({
      search_by_name: '',
    });
    setIsResetDisabled(true);
  };

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

  const convertToCSV = (data: any[]) => {
    const header = ['Code', 'Description', 'GST', 'Rate', 'HSN Code', 'UOM'];
    const csvRows = [header.join(',')];
    for (const item of data) {
      const rowData = [
        item.item_name,
        item.description,
        item.gst.rate,
        item.rate || '-',
        item.hsn_code.code,
        item.uom.name,
      ];
      csvRows.push(rowData.join(','));
    }
    return csvRows.join('\n');
  };

  const fetchAllData = async () => {
    const itemData: any = {
      limit: getFilterData.total_count,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
    };
    const response = await addProduct.filterItem(itemData);
    return response.content;
  };

  const handleDownload = async () => {
    const allData = await fetchAllData();
    const csvContent = convertToCSV(allData);
    const blob = new Blob([csvContent], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'data.csv';
    link.click();
    URL.revokeObjectURL(url);
  };

  const handleEdit = (id: any) => {
    navigate(`/product-edit/${id}`);
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.container}>
      <CustomLoader loading={searchLoader} size={48} color="#333C44">
        <div className={Styles.topContent}>
          <div className={Styles.leftContainer}>
            <h1>List of Items</h1>
            <p>Manage your raw materials ( Raw, Semi Finished & Finished).</p>
          </div>
          <div className={Styles.rightContainer}>
            <div className={Styles.button}>
              <Button
                text={
                  <div style={{ display: 'flex', alignItems: 'center' }}>
                    <DownloadIcon style={{ padding: '4px' }} />
                    Download csv
                  </div>
                }
                onClick={handleDownload}
                backgroundColor="white"
                textColor="black"
                width={140}
                height={45}
                style={{ paddingLeft: '8px' }}
                border="1px solid #D0D5DD"
                borderRadius={5}
              />
            </div>
            <div className={Styles.button}>
              <ButtonOne
                color="primary"
                shape="rectangle"
                justify="center"
                size="medium"
                icon={<AddIcon />}
                onClick={() => {
                  navigate('/product-add');
                }}
              >
                Add Items
              </ButtonOne>
            </div>
          </div>
        </div>

        <div className={Styles.middleContent}>
          <div className={Styles.middleRightContent}>
            <div className={Styles.searchField}>
              <Input
                width="260px"
                prefixIcon={<SearchIcon />}
                name="search_by_name"
                value={filterValues.search_by_name}
                onChange={(e) => handleFilterChange(e)}
                placeholder="Search"
              />
            </div>
            <div>
              <ButtonOne
                className={Styles.searchButton}
                shape="rectangle"
                justify="center"
                size="small"
                onClick={handleSearch}
              >
                Search
              </ButtonOne>
            </div>
            <div>
              <ButtonOne
                className={Styles.resetButton}
                shape="rectangle"
                justify="center"
                size="small"
                onClick={handleReset}
                disabled={isResetDisabled}
              >
                Reset
              </ButtonOne>
            </div>
          </div>
          <div className={Styles.groupButton}>
            <CustomGroupButton
              labels={buttonLabels}
              onClick={handleGroupButtonClick}
              activeButton={activeButton}
            />
          </div>
        </div>
        <div>
          <table>
            <thead>
              <tr>
                <th>S No</th>
                <th>Item Name</th>
                <th>Item Type</th>
                <th>Description</th>
                <th>GST</th>
                <th>Rate</th>
                {activeButton === 'AC' && <th></th>}
              </tr>
            </thead>
            <tbody>
              {getFilterData?.total_count === 0 ? (
                <tr>
                  <td></td>
                  <td></td>
                  <td>No data found</td>
                  {activeButton === 'AC' && <td></td>}
                </tr>
              ) : (
                getFilterData?.content?.map((data: any, index: any) => (
                  <tr key={data.item_id}>
                    <td>{startingIndex + index}</td>
                    <td>{data.item_name}</td>
                    <td>{data.item_type && data.item_type.master_data_name}</td>
                    <td>{data.description}</td>
                    <td>{data.gst.rate}</td>
                    <td>{formatBudgetValue(data.rate || '-')}</td>
                    {activeButton === 'AC' && (
                      <td>
                        <div className={Styles.tablerow}>
                          <EditIcon onClick={() => handleEdit(data.item_id)} />
                        </div>
                      </td>
                    )}
                  </tr>
                ))
              )}
            </tbody>
          </table>
        </div>
        <div className={Styles.pagination}>
          <Pagination
            currentPage={currentPage}
            totalPages={getFilterData?.total_page}
            totalCount={getFilterData?.total_count}
            rowsPerPage={rowsPerPage}
            onPageChange={handlePageChange}
            onRowsPerPageChange={handleRowsPerPageChange}
          />
        </div>
      </CustomLoader>
    </div>
  );
};

export default ProductPage;
