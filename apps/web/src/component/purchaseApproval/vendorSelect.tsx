import React, { useState, useEffect } from 'react';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import Button from '../ui/Button';
import Pagination from '../menu/pagination';
import CustomLoader from '../ui/customLoader';
import { formatBudgetValue } from '../../helper/common-function';
import { useNavigate } from 'react-router-dom';
import { useParams } from 'react-router-dom';
import Styles from '../../styles/vendorSelect.module.scss';
import CustomEditDialog from '../ui/customEditDialogBox';
import vendorQuotesService from '../../service/vendorQuotes-service';

const VendorSelect = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const prID = Number(routeParams?.id);
  const [tableData, setTableData] = useState([]);
  const [dataCount, setDataCount] = useState(0);
  const [dataLoading, setDataLoading] = useState(false);
  const [Id, setID] = useState();
  const [mode, setMode] = useState('');
  const [open, setOpen] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const vendorData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    purchase_request_id: prID,
  };
  useEffect(() => {
    const getAllData = async () => {
      try {
        setDataLoading(true);
      } finally {
        const result = await vendorQuotesService.vendorQuotesData(vendorData);
        console.log('vdata', result);

        if (result.message === 'success') {
          setTableData(result.content);
          setDataLoading(false);
        }
      }
    };
    getAllData();
  }, []);

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div className={Styles.container}>
      <div className={Styles.textContent}>
        <h3>Vendor Detail List</h3>
        <span className={Styles.content}>Select the apt vendor</span>
      </div>
      <div className={Styles.dividerStyle}></div>
      <div className={Styles.tableContainer}>
        <table>
          <thead>
            <tr>
              <th>S No</th>
              <th>Vendor Name </th>
              <th>No of Items</th>
              <th>Budget</th>
              <th>Quoatation Status</th>
              <th>Document</th>
              <th>Options</th>
            </tr>
          </thead>
          <tbody>
            {tableData?.map((data: any, index: number) => {
              const totalQuantity = data.quotation_details.reduce(
                (total:any, item:any) => total + item.quantity,
                0
              );
              return (
                <tr key={data.vendor_quotes_id}>
                  <td>{startingIndex + index}</td>
                  <td>{data.vendor_name}</td>
                  <td>{totalQuantity}</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
    </div>
  );
};
export default VendorSelect;
