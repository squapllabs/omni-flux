import React, { useState, useEffect, useRef } from 'react';
import { useParams, useNavigate, useLocation } from 'react-router-dom';
import { formatBudgetValue } from '../../../../helper/common-function';
import Styles from '../../../../styles/newStyles/localPurchase.module.scss';
import indentApprovalRequestService from '../../../../service/indent-approval-request-service';
import ProjectSubheader from '../../projectSubheader';
import SiteNavigateIcon from '../../../menu/icons/siteNavigateIcon';
// import CloseIcon from '../../../menu/icons/closeIcon';
// import userService from '../../../../service/user-service';
// import FileUploadIcon from '../../../menu/icons/fileUploadIcon';
import Button from '../../../ui/Button';
import { usePurchaseOrderRequest } from '../../../../hooks/purchase-request-hooks';
import CustomSnackBar from '../../../ui/customSnackBar';
import ApproveDialogBox from '../../../ui/CustomApprovePopup';

const LocalPurchaseOrder = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const location = useLocation();
  const projectId = location.state.project_id;
  const indentId = Number(routeParams?.id);
  const [indentRequestData, setIndentRequestData] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(50);
  // const [purchaseOrderDocument, setPurchaseOrderDocument] = useState<any>([]);
  // const [fileSizeError, setFileSizeError] = useState<string>('');
  // const [fileMandatoryError, setFileMandatoryError] = useState('');
  // const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  // const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  // const [fieldValue, setFieldValue] = useState<any>([]);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openApprove, setOpenApprove] = useState(false);
  // const fileInputRef = useRef<HTMLInputElement | null>(null);
  // const onButtonClick = () => {
  //   if (fileInputRef.current) {
  //     fileInputRef.current.click();
  //   }
  // };
  const { mutate: postDataForFilter } = usePurchaseOrderRequest();
  const currentDate = new Date();

  const fetchData = async () => {
    const indentData = await indentApprovalRequestService.getOneIndentById(
      indentId
    );
    setIndentRequestData(indentData);
  };

  useEffect(() => {
    fetchData();
  }, [indentId]);

  // const deleteFileinList = (index: any) => {
  //   let tempObj = {};
  //   purchaseOrderDocument[index].is_delete = 'Y';
  //   tempObj = {
  //     ...purchaseOrderDocument[index],
  //   };
  //   let tempArry = [...purchaseOrderDocument];
  //   tempArry[index] = tempObj;
  //   setPurchaseOrderDocument(tempArry);
  // };

  // const handleFileSelect = async (e: any) => {
  //   const files = e.target.files;
  //   if (files.length > 0) {
  //     const fileList: File[] = Array.from(files);
  //     const oversizedFiles = fileList.filter(
  //       (file) => file.size > 10 * 1024 * 1024
  //     );
  //     if (oversizedFiles.length > 0) {
  //       const oversizedFileNames = oversizedFiles
  //         .map((file) => file.name)
  //         .join(', ');
  //       const errorMessage = `The following files exceed 10MB: ${oversizedFileNames}`;
  //       setFileSizeError(errorMessage);
  //       setMessage(errorMessage);
  //     } else {
  //       const selectedFilesArray: File[] = [];
  //       const selectedFileNamesArray: string[] = [];
  //       const arr: any = [];
  //       for await (const file of fileList) {
  //         const code = 'purchase-order-item-' + indentId;
  //         const response = await userService.documentUpload(file, code);
  //         const obj = {
  //           ...response?.data[0],
  //           is_delete: 'N',
  //           code: code,
  //           folder: 'purchase-order-item',
  //         };
  //         arr.push(obj);
  //         selectedFilesArray.push(file);
  //         selectedFileNamesArray.push(file?.name);
  //       }
  //       setPurchaseOrderDocument(arr);
  //       setFieldValue({ ...fieldValue, vendor_quotes_documents: arr });
  //       setSelectedFiles(selectedFilesArray);
  //       setSelectedFileName(selectedFileNamesArray);
  //       setFileSizeError('');
  //       setMessage('Document uploaded');
  //       setOpenSnack(true);
  //     }
  //   }
  // };

  // const generateCustomQuotationName = () => {
  //   if (indentRequestData) {
  //     const name = indentRequestData?.data?.indent_request_code;
  //     const year = new Date().getFullYear();
  //     const customBillName = `ALM-LP-${name.slice(-5)}-${year}`;
  //     return customBillName.toUpperCase();
  //   }
  //   return '';
  // };

  const handleConvertToPo = () => {
    const purchaseOrderItems: any = [];
    indentRequestData?.data?.indent_request_details?.forEach(
      (data: any, index: any) => {
        purchaseOrderItems.push({
          item_id: data?.bom_detail_data?.item_id,
          order_quantity: data?.indent_requested_quantity,
          inward_quantity: data?.indent_requested_quantity,
          unit_price: data?.bom_detail_data?.rate,
        });
      }
    );
    const purchaseOrderData = {
      indent_request_id: indentId,
      purchase_order_type: 'Local Purchase',
      total_cost: indentRequestData?.data?.total_cost,
      order_remark: 'Item Purchased locally',
      status: 'Processing',
      order_date: currentDate,
      purchase_order_item: purchaseOrderItems,
      // purchase_order_documents: fieldValue?.vendor_quotes_documents,
    };
    postDataForFilter(purchaseOrderData, {
      onSuccess: (data, variables, context) => {
        if (data?.message === 'success') {
          setMessage('Purchase Order Create Successfull');
          setOpenSnack(true);
          setTimeout(() => {
            navigate(`/project-edit/${projectId}`);
          }, 1000);
        }
      },
    });
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleCloseApprove = () => {
    setOpenApprove(false);
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div>
      <div>
        <ProjectSubheader
          title="Local Purchase Order"
          navigation={`/project-edit/${projectId}`}
          description=""
        />
      </div>
      <div className={Styles.sub_header}>
        <div style={{ display: 'flex' }}>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              padding: '20px 10px 20px 20px',
            }}
          >
            <div className={Styles.textContent_1}>
              <span className={Styles.projectTitle}>Expense Code</span>
              <h3>{indentRequestData?.data?.indent_request_code}</h3>
            </div>
          </div>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              padding: '20px 10px 20px 20px',
            }}
          >
            <div className={Styles.textContent_1}>
              <span className={Styles.projectTitle}>Project</span>
              <h3>{indentRequestData?.data?.project_data?.project_name}</h3>
            </div>
          </div>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div style={{ display: 'flex', alignItems: 'center', gap: '20px' }}>
            <div>
              <SiteNavigateIcon width={30} height={30} />
            </div>
            <div className={Styles.textContent_1}>
              <span className={Styles.projectTitle}>Site </span>
              <h3>{indentRequestData?.data?.site_data?.name}</h3>
            </div>
          </div>
        </div>
        <div className={Styles.boqAmount}>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              gap: '10px',
              padding: '20px 10px 20px 10px',
            }}
          >
            <div className={Styles.countContent}>
              <h3>
                {formatBudgetValue(
                  indentRequestData?.data?.total_cost
                    ? indentRequestData?.data?.total_cost
                    : 0
                )}
              </h3>
              <span className={Styles.countContentTitle}>Total Cost</span>
            </div>
          </div>
        </div>
      </div>
      <div className={Styles.selected}></div>
      <div>
        <div className={Styles.tableContainer}>
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
              {indentRequestData?.data?.indent_request_details?.map(
                (data: any, index: any) => {
                  return (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.bom_detail_data?.item_data?.item_name}</td>
                      <td>{data?.bom_detail_data?.uom_data?.name}</td>
                      <td>{data?.indent_requested_quantity}</td>
                      <td>{formatBudgetValue(data?.total)}</td>
                    </tr>
                  );
                }
              )}
            </tbody>
          </table>
        </div>
      </div>
      {/* <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          gap: '10px',
          padding: '20px',
          justifyContent: 'space-between',
        }}
      >
        <div>
          {purchaseOrderDocument?.length > 0 &&
          purchaseOrderDocument[0].is_delete === 'N' ? (
            <div>
              {purchaseOrderDocument?.map((document: any, index: any) => {
                const customQuotationName = generateCustomQuotationName();
                if (document.is_delete === 'N')
                  return (
                    <div
                      key={document.code}
                      style={{
                        width: '250px',
                        cursor: 'pointer',
                        fontWeight: 'bolder',
                        color: 'blue',
                        display: 'flex',
                        fontSize: '15px',
                      }}
                    >
                      <div>{customQuotationName}</div>
                      <CloseIcon
                        width={50}
                        height={10}
                        onClick={() => deleteFileinList(index)}
                      />
                    </div>
                  );
              })}
            </div>
          ) : (
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                gap:'10px'
              }}
            >
              <div className={Styles.uploadLabel}>
                Upload Bill<span style={{ color: 'red' }}>*</span>
              </div>
              <div title="Attach document">
                <input
                  ref={fileInputRef}
                  id="upload-photo"
                  name="upload_photo"
                  type="file"
                  style={{ display: 'none' }}
                  onChange={(e) => handleFileSelect(e)}
                />
                <div
                  style={{
                    cursor: 'pointer',
                    paddingBottom: '5px',
                  }}
                  onClick={() => {
                    onButtonClick();
                  }}
                >
                  <FileUploadIcon color="#7f56d9" />
                </div>
                {fileMandatoryError && (
                  <div className={Styles.documentErr}>{fileMandatoryError}</div>
                )}
              </div>
            </div>
          )}
        </div>
        <div className={Styles.button}>
          <Button
            shape="rectangle"
            justify="center"
            size="small"
            color="primary"
            onClick={() => handleConvertToPo()}
          >
            Convert To Po
          </Button>
        </div>
      </div> */}
      {indentRequestData?.data?.approver_status === 'Approved' ? (
        <div className={Styles.button}>
          <Button
            shape="rectangle"
            justify="center"
            size="small"
            color="primary"
            onClick={() => setOpenApprove(true)}
          >
            Convert To Po
          </Button>
        </div>
      ) : (
        <div className={Styles.button}>
          <span className={Styles.message}>Moved to PO</span>
        </div>
      )}
      <ApproveDialogBox
        open={openApprove}
        title="Move Local Purchase to Purchase Order"
        contentLine1="Are yoy sure want to move this to purchase order ?"
        contentLine2=""
        handleClose={handleCloseApprove}
        handleConfirm={() => handleConvertToPo()}
      />
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={2000}
        type="success"
      />
    </div>
  );
};
export default LocalPurchaseOrder;
