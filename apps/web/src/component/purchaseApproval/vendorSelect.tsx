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
import EditIcon from '../menu/icons/editIcon';
import PurchaseRequestEdit from './purchaseRequestEdit';
import StarIcon from '../menu/icons/starIcon';
import { updateVendorQuotes } from '../../hooks/vendorQuotes-hooks';
import BackArrowIcon from '../menu/icons/backArrow';
// import purchaseRequestService from '../../service/purchaseRequest-service';
// import { updatePurchaseRequest } from '../../hooks/purchase-request-hooks';

const VendorSelect = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const { mutate: updateOneVendorQuotes } = updateVendorQuotes();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const prID = Number(routeParams?.id);
  const [tableData, setTableData] = useState([]);
  const [dataLoading, setDataLoading] = useState(false);
  const [Id, setID] = useState();
  const [mode, setMode] = useState('');
  const [open, setOpen] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [reload, setReload] = useState(false);
  

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
  }, [reload]);

  const handleEdit = (value: any) => {
    setMode('EDIT');
    setID(value);
    setOpen(true);
    setReload(false);
  };

  const handleApprove = (value: any) => {
    setID(value);
    handleSubmit(value);
  };

  const handleSubmit = async (id: any) => {
    try {
      const data = await vendorQuotesService.getOneVendorQuotesById(id);
      console.log('data', data);
      const obj = {
        vendor_quotes_id: data?.data?.vendor_quotes_id,
        purchase_request_id: data?.data?.purchase_request_id,
        vendor_id: data?.data?.vendor_id,
        quotation_status: 'Approved',
        updated_by: userID,
      };
      updateOneVendorQuotes(obj, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            navigate('/purchase-view');
          }
        },
      });
    } catch {
      console.log('data');
    }
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
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
                <th>Quotation Status</th>
                <th>Document</th>
                <th></th>
                <th></th>
              </tr>
            </thead>
            <tbody>
              {tableData?.map((data: any, index: number) => {
                const isQuotationPending = data.quotation_status === 'Pending';
                const isQuotationApproved =
                  data.quotation_status === 'Approved';
                return (
                  <tr key={data.vendor_quotes_id}>
                    <td>{startingIndex + index}</td>
                    <td>{data.vendor_name}</td>
                    <td>{data?.quotation_details?.length}</td>
                    <td>{data.total_quotation_amount}</td>
                    <td>{data.quotation_status}</td>
                    <td>
                      {data.vendor_quotes_documents?.map(
                        (document: any, index: any) => (
                          <li key={index}>
                            <a
                              href={document.path}
                              target="_blank"
                              rel="noopener noreferrer"
                            >
                              Document {index + 1}
                            </a>
                          </li>
                        )
                      )}
                    </td>
                    <td>
                      {isQuotationApproved ? (
                        ''
                      ) : (
                        <EditIcon
                          onClick={() => handleEdit(data.vendor_quotes_id)}
                        />
                      )}
                       {isQuotationPending ? (
                        '' 
                      ) : isQuotationApproved ? (
                        <StarIcon style={{ cursor: 'not-allowed' }} />
                      ) : (
                        <StarIcon
                          onClick={() => handleApprove(data.vendor_quotes_id)}
                        />
                      )}
                    </td>
                    
                  </tr>
                );
              })}
            </tbody>
          </table>
          <div className={Styles.button}>
            <Button
              shape="rectangle"
              justify="center"
              size="small"
              color="primary"
              icon={<BackArrowIcon />}
              onClick={() => navigate('/purchase-view')}
            >
              Back
            </Button>
          </div>
        </div>
        <CustomEditDialog
          open={open}
          content={
            <PurchaseRequestEdit
              setOpen={setOpen}
              open={open}
              mode={mode}
              setReload={setReload}
              vendorID={Id}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
            />
          }
        />
      </CustomLoader>
    </div>
  );
};
export default VendorSelect;
