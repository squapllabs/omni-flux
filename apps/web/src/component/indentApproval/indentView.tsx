import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import CustomSnackBar from '../ui/customSnackBar';
import BackArrowIcon from '../menu/icons/backArrow';
import {
  useGetAllIndentRequestDetail,
  updateIndentRequest,
} from '../../hooks/indent-approval-hooks';
import Styles from '../../styles/indentList.module.scss';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import CustomLoader from '../ui/customLoader';
import CustomRejectPopup from '../ui/CustomRejectCommentPopup';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import indentApprovalService from '../../service/indent-approval-request-service';

const IndentView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(50);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [showRejectForm, setShowRejectForm] = useState(false);
  const IndentId = Number(routeParams?.id);
  const [tableData, setTableData] = useState([]);
  const [dataLoading, setDataLoading] = useState(false);
  const masterData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    indent_request_id: Number(routeParams?.id),
  };
  // const {  isLoading: dataLoading } =
  //   useGetAllIndentRequestDetail(masterData);

  useEffect(() => {
    const getAllData = async () => {
      try {
        setDataLoading(true);
      } finally {
        const result = await indentApprovalService.indentDetailData(masterData);
        if (result.message === 'success') {
          setTableData(result.content);
          setDataLoading(false);
        }
      }
    };
    getAllData();
  }, []);

  const { mutate: updateIndentRequestData } = updateIndentRequest();
  const handleApprove = () => {
    const date = format(new Date(), 'yyyy/MM/dd');
    const obj = {
      indent_request_id: IndentId,
      approver_status: 'Approved',
      approved_date: date,
      rejected_date: null,
      updated_by: userID,
      approver_user_id: userID,
    };
    updateIndentRequestData(obj, {
      onSuccess: (data, variables, context) => {
        if (data?.status === true) {
          setMessage('Approved Successfully');
          setOpenSnack(true);
          setTimeout(() => {
            navigate('/indent-view');
          }, 1000);
        }
      },
    });
  };

  const handleReject = () => {
    setShowRejectForm(true);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.box}>
          <div className={Styles.headingTop}>
            <div className={Styles.textContent}>
              <h3>Indent Request Detail List</h3>
              <span className={Styles.content}>
                Manage your Indent raise detail across your project
              </span>
            </div>
            <div className={Styles.backButton}>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                icon={<BackArrowIcon />}
                onClick={() => navigate('/indent-view')}
              >
                Back
              </Button>
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <div>
              <table>
                <thead>
                  <tr>
                    <th>S No</th>
                    <th>Item Name </th>
                    <th>UOM</th>
                    <th>Quantity</th>
                    <th>Total Cost</th>
                  </tr>
                </thead>
                <tbody>
                  {tableData?.map((data: any, index: number) => {
                    return (
                      <tr key={data.indent_request_id}>
                        <td>{startingIndex + index}</td>
                        <td>{data?.bom_detail_data?.item_data?.item_name}</td>
                        <td>{data?.bom_detail_data?.uom_data?.name}</td>
                        <td>{data?.quantity}</td>
                        <td>{formatBudgetValue(data?.total)}</td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
          <div className={Styles.approveButtons}>
            <div>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                onClick={() => handleApprove()}
              >
                Approve
              </Button>
            </div>
            <div>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="secondary"
                onClick={() => handleReject()}
              >
                Reject
              </Button>
            </div>
          </div>
        </div>
        <CustomRejectPopup
          isVissible={showRejectForm}
          onAction={setShowRejectForm}
          selectedIndentId={IndentId}
        />
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </CustomLoader>
    </div>
  );
};

export default IndentView;
