import React, { useState, useEffect } from 'react';
import Button from '../ui/Button';
import ButtonOne from '../menu/button';
import { useNavigate } from 'react-router-dom';
import Styles from '..//../styles/listItem.module.scss';
import Pagination from '../menu/CustomPagination';
import AddIcon from '../menu/icons/addIcon';
import DownloadIcon from '../menu/icons/download';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import {
  useGetAllPaginatedItemData
} from '../../hooks/add-product-hooks';
import EditIcon from '../menu/icons/newEditIcon';
import { formatBudgetValue } from '../../helper/common-function';
import addProduct from '../../service/add-product';
import FilterOrderIcon from '../menu/icons/filterOrderIcon';
import { handleSortByColumn } from './../../helper/common-function'

const ProductPage = () => {

  const navigate = useNavigate();
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [sortColumn, setSortColumn] = useState('');
  const [sortOrder, setSortOrder] = useState('desc');
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const itemData: any = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: sortColumn === '' ? 'created_date' : sortColumn,
    order_by_direction: sortOrder,
    status: activeButton,
    global_search: filterValues.search_by_name,
  };
  const {
    isLoading: searchLoader,
    data: getFilterData,
    refetch,
  } = useGetAllPaginatedItemData(itemData);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton, sortColumn, sortOrder]);

  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

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
        {getFilterData?.is_available ? (
          <div>
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                <div className={Styles.subHeading}>
                  <h3>ITEMS</h3>
                </div>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon color="white" />}
                    onClick={() => {
                      navigate('/product-add');
                    }}
                  >
                    Add Item
                  </Button>
                </div>
                <div className={Styles.button}>
                  <ButtonOne
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
                    height={38}
                    style={{ paddingLeft: '8px' }}
                    border="1px solid #D0D5DD"
                    borderRadius={5}
                  />
                </div>
              </div>
              <div className={Styles.filters}>
                <div className={Styles.searchBar}>
                  <Input
                    placeholder="Search Items"
                    width="300px"
                    prefixIcon={<SearchIcon />}
                    name="filter_value"
                    onChange={(e) => {
                      setFilterValues({
                        ...filterValues,
                        ['search_by_name']: e.target.value,
                      });
                      setCurrentPage(1);
                    }}
                  />
                </div>
              </div>
            </div>
            <div className={Styles.tableContainer}>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>#</th>
                    <th
                      onClick={() => handleSortByColumn('item_name', sortOrder, setSortOrder, setSortColumn)}
                    >
                      <div className={Styles.headingRow}>
                        <div>Item Name</div><div>
                          <FilterOrderIcon />
                        </div>
                      </div>
                    </th>
                    <th>Item Type</th>
                    <th>Description</th>
                    <th>GST</th>
                    <th
                      onClick={() => handleSortByColumn('rate', sortOrder, setSortOrder, setSortColumn)}
                    >
                      <div className={Styles.headingRow}>
                        <div>Rate</div><div>
                          <FilterOrderIcon />
                        </div>
                      </div>
                    </th>
                    <th>Action</th>
                    {activeButton === 'AC' && <th></th>}
                  </tr>
                </thead>
                <tbody>
                  {getFilterData?.total_count === 0 ? (
                    <tr>
                      <td></td>
                      <td></td>
                      <td></td>
                      <td>No data found</td>
                      {activeButton === 'AC' && <td></td>}
                    </tr>
                  ) : (
                    getFilterData?.content?.map((data: any, index: any) => (
                      <tr key={data?.item_id}>
                        <td>{startingIndex + index}</td>
                        <td>{data?.item_name}</td>
                        <td>{data?.item_type && data?.item_type.master_data_name}
                          <span>
                            {data?.item_type && data?.item_type.master_data_name
                              ? data?.item_type && data?.item_type.master_data_name.substring(0, 50)
                              : '-'}
                          </span>
                        </td>
                        <td>{data?.description}</td>
                        <td>{data?.gst?.rate}</td>
                        <td>{formatBudgetValue(data.rate || '-')}</td>
                        {activeButton === 'AC' && (
                          <td>
                            <div className={Styles.tablerow}>
                              <EditIcon onClick={() => handleEdit(data?.item_id)} />
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

          </div>
        ) : (
          <div>
            <div className={Styles.subHeading}>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div>
                <img
                  src="/items.jpg"
                  alt="aa"
                  width="100%"
                  height="200px"
                  style={{ paddingTop: '35px', paddingBottom: '15px' }}
                />
              </div>
              <div>
                <h5>Items list is Empty</h5>
              </div>
              <div className={Styles.contentGap}>
                <span className={Styles.spanContent}>Go ahead, add new Items</span>
              </div>
              <div className={Styles.emptyButton}>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => {
                    navigate('/product-add');
                  }}
                >
                  Add Item
                </Button>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
    </div>
  );
};

export default ProductPage;
