import React, { useState, useEffect } from 'react';
import Button from '../menu/button';
import { useNavigate } from 'react-router-dom';
import Styles from '..//../styles/listItem.module.scss';
import Pagination from './pagination';
import AddIcon from '../menu/icons/addIcon';
import DownloadIcon from '../menu/icons/download';
import Dropdown from '../menu/dropDown';
import CancelFilterIcon from '../menu/icons/cancelFilterIcon';
import FilterIcon from '../menu/icons/filterIcon';
import DescendingIcon from '../menu/icons/descendingIcon';
import SearchInput from './searchInputBox';
import SearchIcon from '../menu/icons/search';

import items from '../../service/add-product';

const ProductPage = () => {
  const navigate = useNavigate();

  const [itemValue, setItemValues] = useState([]);
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(5); // Set initial value to 1
  const [rowsPerPage, setRowsPerPage] = useState(5);

  useEffect(() => {
    fetchData();
  }, [rowsPerPage, currentPage]);

  const handleAddProduct = () => {
    navigate('/add-products');
  };

  const fetchData = async () => {
    const requestData = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
    };
    // console.log(currentPage);
    console.log(requestData.offset);
    try {
      const data = await items.getAllItems(requestData);

      setItemValues(data.data);
      setTotalPages(data.total_page);

      // console.log(data.total_page);
    } catch (error) {
      console.log('Error in fetching data:', error);
    }
  };

  const handlePageChange = (page) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (newRowsPerPage) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  const convertToCSV = (data: any[]) => {
    const header = [
      'Code',
      'Sub Category',
      'Description',
      'GST',
      'HSN Code',
      'UOM',
    ];
    const csvRows = [header.join(',')];
    for (const item of data) {
      const rowData = [
        item.item_name,
        item.sub_sub_category.name,
        item.description,
        item.gst.rate,
        item.hsn_code.code,
        item.uom.name,
      ];
      csvRows.push(rowData.join(','));
    }
    return csvRows.join('\n');
  };

  const handleDownload = () => {
    const csvContent = convertToCSV(itemValue);
    const blob = new Blob([csvContent], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'data.csv';
    link.click();
    URL.revokeObjectURL(url);
  };

  const handleReset = () => {
    console.log('reset');
  };
  const handleSearch = () => {};
  return (
    <div className={Styles.container}>
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
              border="1px solid #D0D5DD"
              borderRadius={8}
            />
          </div>
          <div className={Styles.button}>
            <Button
              text={
                <div style={{ display: 'flex', alignItems: 'center' }}>
                  <AddIcon style={{ padding: '4px' }} />
                  Add items
                </div>
              }
              onClick={handleAddProduct}
              backgroundColor="#7F56D9"
              fontSize={14}
              fontWeight={500}
              width={125}
            />
          </div>
        </div>
      </div>

      <div className={Styles.middleContent}>
        <div className={Styles.middleRightContent}>
          <div
            style={{
              border: '1px solid gray',
              padding: '8px',
              borderRadius: '8px',
              width: '300px',
              display: 'flex',
              alignItems: 'center',
            }}
          >
            <SearchIcon style={{ padding: '0 5px' }} />
            <SearchInput
              placeholder="Search by Item Name"
              onSearch={handleSearch}
            />
          </div>
          <div>
            <Button
              text="Search"
              onClick={() => {}}
              border="1px solid #E9D7FE"
              backgroundColor="#F9F5FF"
              textColor="#6941C6"
              fontWeight={600}
              borderRadius={8}
            />
          </div>

          <div className={Styles.resetButton} onClick={handleReset}>
            <h2> Reset</h2>
          </div>
        </div>

        <div className={Styles.middleRightContent}>
          <div className={Styles.button}>
            <Dropdown
              label={
                <Button
                  text={
                    <div
                      style={{
                        display: 'flex',
                        alignItems: 'center',
                        justifyContent: 'center',
                      }}
                    >
                      <FilterIcon style={{ padding: '0 10px' }} />
                      <p style={{ padding: '0 10px', fontSize: '16px' }}>
                        Filter
                      </p>
                      <CancelFilterIcon
                        style={{
                          padding: '8px 10px',
                          borderLeft: '1px solid #D0D5DD',
                        }}
                      />
                    </div>
                  }
                  onClick={() => {}}
                  width={150}
                  textColor="black"
                  backgroundColor="white"
                  border="1px solid #D0D5DD"
                  borderRadius={8}
                />
              }
            >
              <div>filter item</div>
            </Dropdown>
          </div>

          <div className={Styles.button}>
            <Dropdown
              label={
                <Button
                  text={
                    <div
                      style={{
                        display: 'flex',
                        alignItems: 'center',
                        justifyContent: 'center',
                      }}
                    >
                      <DescendingIcon style={{ padding: '0 10px' }} />
                      <p
                        style={{
                          padding: '8px 10px',
                          fontSize: '16px',
                          borderLeft: '1px solid #D0D5DD',
                        }}
                      >
                        Last Updated On
                      </p>
                    </div>
                  }
                  onClick={() => {}}
                  width={190}
                  textColor="black"
                  backgroundColor="white"
                  border="1px solid #D0D5DD"
                  borderRadius={8}
                />
              }
            ></Dropdown>
          </div>
        </div>
      </div>
      <div>
        <table>
          <thead>
            <tr>
              <th>Code</th>
              <th>Sub Category</th>
              <th>Description</th>
              <th>GST</th>
              <th>HSN Code</th>
              <th>UOM</th>
              {/* <th>Delete</th> */}
            </tr>
          </thead>
          <tbody>
            {itemValue.map((item) => (
              <tr>
                <td>{item.item_name}</td>
                <td>{item.sub_sub_category.name}</td>
                <td>{item.description}</td>
                <td>{item.gst.rate}</td>
                <td>{item.hsn_code.code}</td>
                <td>{item.uom.name}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      <Pagination
        currentPage={currentPage}
        totalPages={totalPages}
        rowsPerPage={rowsPerPage}
        onPageChange={handlePageChange}
        onRowsPerPageChange={handleRowsPerPageChange}
      />
    </div>
  );
};

export default ProductPage;
