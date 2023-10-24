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
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [showRejectForm, setShowRejectForm] = useState(false);
  const IndentId = Number(routeParams?.id);
  const masterData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    indent_request_id: IndentId,
  };
  const {
    data: getAllData,
    isLoading: dataLoading,
    refetch,
  } = useGetAllIndentRequestDetail(masterData);
  // console.log('getAAAAAAAAAAAAAAAAA', getAllData);

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
  useEffect(() => {
    refetch();
  }, []);

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
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th className={Styles.tableHeading}>#</th>
                    <th className={Styles.tableHeading}>Item Name </th>
                    <th className={Styles.tableHeading}>UOM</th>
                    <th className={Styles.tableHeading}>Quantity</th>
                    <th className={Styles.tableHeading}>Cost</th>
                  </tr>
                </thead>
                <tbody>
                  {getAllData?.total_count === 0 ? (
                    <tr>
                      <td></td>
                      <td></td>
                      <td>No data found</td>
                    </tr>
                  ) : (
                    ''
                  )}
                  {getAllData?.content?.map((data: any, index: number) => {
                    return (
                      <tr key={data.indent_request_id}>
                        <td>{startingIndex + index}</td>
                        <td>{data?.bom_detail_data?.item_data?.item_name}</td>
                        <td>{data?.bom_detail_data?.uom_data?.name}</td>
                        <td>{data?.indent_requested_quantity}</td>
                        <td>{formatBudgetValue(data?.total)}</td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
          {getAllData?.content[0]?.indent_request_data?.approver_status ===
            'Approved' ||
          getAllData?.content[0]?.indent_request_data?.approver_status ===
            'Rejected' ? null : ( 
            <div className={Styles.approveButtons}>
              <div>
                <Button
                  shape="rectangle"
                  justify="center"
                  size="small"
                  color="primary"
                  onClick={() => handleApprove()}
                  disabled={getAllData?.total_count === 0 ? true : false}
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
                  disabled={getAllData?.total_count === 0 ? true : false}
                >
                  Reject
                </Button>
              </div>
            </div>
          )}
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
