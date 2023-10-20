import React, { useState, useEffect } from 'react';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import Button from '../ui/Button';
import CustomLoader from '../ui/customLoader';
import { environment } from '../../environment/environment';
import { formatBudgetValue } from '../../helper/common-function';
import { useNavigate, useLocation } from 'react-router-dom';
import { useParams } from 'react-router-dom';
import Styles from '../../styles/vendorSelect.module.scss';
import CustomEditDialog from '../ui/customEditDialogBox';
import vendorQuotesService from '../../service/vendorQuotes-service';
import PurchaseRequestEdit from './purchaseRequestEdit';
import { updateVendorQuotes } from '../../hooks/vendorQuotes-hooks';
import BackArrowIcon from '../menu/icons/backArrow';
import CustomMenu from '../ui/CustomMenu';
import CustomSnackBar from '../ui/customSnackBar';
import { Link } from 'react-router-dom';
import PreviousPageIcon from '../menu/icons/previousPageIcon';

const VendorSelect = () => {
  const routeParams = useParams();
  const location = useLocation();
  const indentId = location.state.indent_id;
  const projectId = location.state.project_id;
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
  const [isAnyRowApproved, setIsAnyRowApproved] = useState(false);

  const nullLableNameFromEnv = `${environment.NULLVALUE}`;

  const vendorData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'asc',
    status: 'AC',
    global_search: '',
    purchase_request_id: prID,
  };
  useEffect(() => {
    const getAllData = async () => {
      try {
        setDataLoading(true);
        setIsAnyRowApproved(false);
      } finally {
        const result = await vendorQuotesService.vendorQuotesData(vendorData);

        if (result.message === 'success') {
          setTableData(result.content);
          if (
            result.content.some(
              (data: any) => data.quotation_status === 'Approved'
            )
          ) {
            setIsAnyRowApproved(true);
          }
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

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleApprove = (value: any) => {
    setID(value);
    handleSubmit(value);
  };

  const handleSubmit = async (id: any) => {
    try {
      const data = await vendorQuotesService.getOneVendorQuotesById(id);
      const obj = {
        vendor_quotes_id: data?.data?.vendor_quotes_id,
        purchase_request_id: data?.data?.purchase_request_id,
        vendor_id: data?.data?.vendor_id,
        quotation_status: 'Approved',
        updated_by: userID,
        vendor_quotes_documents: data?.data?.vendor_quotes_documents,
        total_quotation_amount: data?.data?.total_quotation_amount,
      };
      updateOneVendorQuotes(obj, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Vendor Approved');
            setOpenSnack(true);
            navigate(`/purchase-detail/${indentId}`, {
              state: { project_id: projectId },
            });
          }
        },
      });
    } catch {
      console.log('Error occured in vendor select ');
    }
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        {/* <div className={Styles.textContent}>
          <h3>Vendor Detail List</h3>
          <span className={Styles.content}>Select the apt vendor</span>
        </div> */}
        <div className={Styles.sub_header}>
          <div
            className={Styles.logo}
            onClick={() => {
              navigate(`/purchase-detail/${indentId}`, {
                state: { project_id: projectId },
              });
            }}
          >
            <PreviousPageIcon width={20} height={20} color="#7f56d9" />
          </div>
          <div style={{ padding: '8px', display: 'flex' }}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',
              width: '700px',
            }}
          >
            <div className={Styles.textContent_1}>
              <h3>Allocated Vendor Detail List</h3>
              <span className={Styles.content}>Select the apt vendor</span>
            </div>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>S No</th>
                {/* <th>Vendor ID</th> */}
                <th>Vendor Name </th>
                <th>No of Items</th>
                <th>Quatation Budget</th>
                <th>Quotation Id</th>
                <th>Quotation Status</th>
                <th>Document</th>
                <th>Options</th>
              </tr>
            </thead>
            <tbody>
              {tableData?.map((data: any, index: number) => {
                const itemData = data?.quotation_details
                const isQuotationPending = data.quotation_status === 'Pending';
                const actions = [
                  {
                    label: 'View Items',
                    onClick: () => {
                      navigate(`/request-items`,{state:{data:itemData,project_id: projectId,indent_id:indentId,page:'VendorPage',purchaseRequestId:prID}});
                    },
                  },
                  {
                    label: 'Edit',
                    onClick: () => {
                      if (!isAnyRowApproved) {
                        handleEdit(data.vendor_quotes_id);
                      }
                    },
                    disabled: isAnyRowApproved,
                  },
                  {
                    label: 'Approve',
                    onClick: () => {
                      if (!isQuotationPending && !isAnyRowApproved) {
                        handleApprove(data.vendor_quotes_id);
                      }
                    },
                    disabled: isQuotationPending || isAnyRowApproved,
                  },
                ];
                const isAnyActionEnabled = actions.some(
                  (action) => !action.disabled
                );
                return (
                  <tr key={data.vendor_quotes_id}>
                    <td>{startingIndex + index}</td>
                    {/* <td>{data.vendor_id || nullLableNameFromEnv}</td> */}
                    <td>{data.vendor_name || nullLableNameFromEnv}</td>
                    <td>
                      {data?.quotation_details?.length || nullLableNameFromEnv}
                    </td>
                    <td>
                      {formatBudgetValue(data.total_quotation_amount) ||
                        nullLableNameFromEnv}
                    </td>
                    <td>
                      <Link to={`/vendor-view-items/${data.vendor_quotes_id}`}>
                        {data.quotation_id}
                      </Link>
                    </td>
                    <td>{data.quotation_status || nullLableNameFromEnv}</td>
                    <td>
                      {data.vendor_quotes_documents?.map(
                        (document: any, index: any) => (
                          <ol key={index}>
                            <a
                              href={document.path}
                              target="_blank"
                              rel="noopener noreferrer"
                            >
                              Document {index + 1}
                            </a>
                          </ol>
                        )
                      ) || nullLableNameFromEnv}
                    </td>
                    <td>
                      {isAnyActionEnabled && <CustomMenu actions={actions} />}
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
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
