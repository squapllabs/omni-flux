import React, { useEffect, useState } from 'react';
import Button from '../ui/Button';
import Styles from '../../styles/purchaseRequestAdd.module.scss';
import { useFormik } from 'formik';
import Input from '../ui/Input';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import AutoCompleteMultiSelect from '../ui/AutoCompleteMultiSelect';
import AddIcon from '../menu/icons/addIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import { useGetAllVendors } from '../../hooks/vendor-hooks';
import { createPurchaseRequest } from '../../hooks/purchaseRequest-hooks';
import PurchaseRequestService from '../../service/purchaseRequest-service';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import { useLocation, useNavigate } from 'react-router-dom';
import CustomSnackBar from '../ui/customSnackBar';
import { getPurchaseRequestCreateValidateyup } from '../../helper/constants/purchaseRequestAdd-constants';
import * as yup from 'yup';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
const PurchaseRequestAdd = () => {
  const [itemValues, setItemsValues] = useState([]);
  const [dynamicitemValues, setDynamicItemsValues] = useState([]);
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const location = useLocation();
  const navigate = useNavigate();
  const projectId = location.state.project_id;
  const indentId = location.state.indent_id;
  let rowIndex = 0;
  const [itemsData, setItemsData] = useState();
  const [dropDisable, setDropDisable] = useState(false);
  const [purchaseRequestData, setPurchaseRequestData] = useState<any>([]);
  const [initialValues, setInitialValues] = useState({
    vendor_id: '',
    item_id: '',
    requested_quantity: '',
    allocated_quantity: '',
    item_name: '',
  });
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');

  const { data: getAllVendorsData = [], isLoading: dropLoading } =
    useGetAllVendors();
  const { mutate: createNewPurchaseRequest } = createPurchaseRequest();
  const validationSchema = getPurchaseRequestCreateValidateyup(yup);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      let arr: any[] = [];
      arr = [...purchaseRequestData, values];
      const newArray = dynamicitemValues.filter(item2 => {
        return !arr.some(item1 => item1.item_name === item2.label);
      });
      setDynamicItemsValues(newArray)
      setPurchaseRequestData(arr);
      resetForm({
        values: {
          ...formik.values,
          item_name: '',
          requested_quantity: '',
          allocated_quantity: '',
        },
      });
      setDropDisable(true);
    },
  });

  const handleDropChange = async () => {
    const itemsData = await PurchaseRequestService.getIndentItems(indentId);
    const arr: any = [];
    setItemsData(itemsData.data);
    const items = itemsData?.data?.map((items: any, index: any) => {
      const obj: any = {
        value: items?.item_id,
        label: items?.item_name,
      };
      arr.push(obj);
    });
    setItemsValues(arr)
    setDynamicItemsValues(arr);
  };

  const deletePurchaseRequest = (index: number) => {
    purchaseRequestData.splice(index, 1);
    setPurchaseRequestData([...purchaseRequestData]);
    const newArray = itemValues.filter(item2 => {
      return !purchaseRequestData.some(item1 => item1.item_name === item2.label);
    });
    setDynamicItemsValues(newArray)
    setDropDisable(false);
  };

  const handleSubmit = () => {
    const requestBody = {
      indent_request_id: indentId,
      requester_user_id: userID,
      request_date: new Date(),
      status: 'Waiting For Quotation',
      project_id: projectId,
      purchase_request_details: purchaseRequestData.map((item: any) => ({
        item_id: item.item_id,
        indent_request_quantity: Number(item.requested_quantity),
        allocated_quantity: Number(item.allocated_quantity),
        item_name: item.item_name,
        rate: Number(item?.rate),
      })),
      vendor_ids: purchaseRequestData.map((item: any) => item.vendor_id.map(Number)).flat()
    };
    createNewPurchaseRequest(requestBody, {
      onSuccess: (data, variables, context) => {
        if (data?.message === 'success') {
          setMessage('Purchase Request created');
          setOpenSnack(true);
          setTimeout(() => {
            navigate(`/purchase-detail/${indentId}`, {
              state: { project_id: projectId },
            });
          }, 1000);
        }
      },
    });
    setPurchaseRequestData('');
  };

  useEffect(() => {
    handleDropChange();
  }, [initialValues]);

  //   const handelOpenVendorForm = () => {
  //     setTimeout(() => {
  //         navigate('/vendor-add', {
  //           state: { project_id: projectId,indentId_id:indentId },
  //         });
  //       }, 1000);
  //   }

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  return (
    <div>
      <div className={Styles.popupContent}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.sub_header}>
            <div
              className={Styles.logo}
              onClick={() => {
                navigate(`/purchase-detail/${indentId}`, {
                  state: { project_id: projectId },
                })
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
                <h4>Request for Quotation</h4>
                <span className={Styles.content}>
                  Raise purchase request against your Project
                </span>
              </div>
            </div>
          </div>

          <div className={Styles.dividerStyle}></div>
          {/* <div className={Styles.inputFields}> */}
          <div className={Styles.fields_container}>
            <div className={Styles.fields_container_2}>
              <div>
                <AutoCompleteSelect
                  label="Items"
                  name="item_id"
                  onChange={formik.handleChange}
                  value={formik.values.item_id}
                  placeholder="Select from options"
                  defaultLabel="Select from options"
                  mandatory
                  width="200px"
                  onSelect={(value) => {
                    formik.setFieldValue('item_id', value);

                    const matchingObjects = itemsData?.filter(
                      (obj: any) => Number(obj.item_id) === Number(value)
                    );
                    formik.setFieldValue(
                      'requested_quantity',
                      matchingObjects[0]?.bom_detail[0]?.indent_request_details[0]?.indent_requested_quantity
                    );
                    formik.setFieldValue(
                      'item_name',
                      matchingObjects[0]?.item_name
                    );
                    formik.setFieldValue(
                      'rate',
                      matchingObjects[0]?.rate
                    );
                  }}
                  optionList={dynamicitemValues}
                  error={formik.touched.item_name && formik.errors.item_name}
                />
              </div>
              <div>
                <Input
                  label="Requested Quantity"
                  placeholder="Enter Quantity"
                  name="requested_quantity"
                  mandatory={true}
                  width="200px"
                  value={formik.values.requested_quantity}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.requested_quantity &&
                    formik.errors.requested_quantity
                  }
                  disabled={true}
                />
              </div>
              <div>
                <Input
                  label="Allocated Quantity"
                  placeholder="Enter Quantity"
                  name="allocated_quantity"
                  mandatory={true}
                  width="200px"
                  value={formik.values.allocated_quantity}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.allocated_quantity &&
                    formik.errors.allocated_quantity
                  }
                />
              </div>

              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  type="submit"
                  icon={<AddIcon color="white" />}
                >
                  Add
                </Button>
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div>
                <AutoCompleteMultiSelect
                  label="Choose Multiple Vendors"
                  name="vendor_id"
                  onChange={formik.handleChange}
                  value={formik.values.vendor_id}
                  placeholder="Select from options"
                  defaultLabel="Select from options"
                  mandatory
                  width="350px"
                  onSelect={(value) => {
                    formik.setFieldValue('vendor_id', value);
                  }}
                  addLabel="Add Vendor"
                  onAddClick={(value) => {
                    navigate('/vendor-add', {
                      state: { project_id: projectId, indent_id: indentId },
                    });
                    // setShowClientForm(true);
                  }}
                  optionList={getAllVendorsData}
                  disabled={dropDisable}
                  error={formik.touched.vendor_id && formik.errors.vendor_id}
                />
              </div>
              {/* <div
                className={Styles.instantAdd}
                onClick={() =>
                  navigate('/vendor-add', {
                    state: { project_id: projectId, indent_id: indentId },
                  })
                }
              >
                <AddIcon style={{ height: '15px', width: '15px' }} />
                <h4 className={Styles.addtext}> Add Vendor</h4>
              </div> */}
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <div>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th className={Styles.tableHeading}>#</th>
                    <th className={Styles.tableHeading}>Item</th>
                    <th className={Styles.tableHeading}>Requested Quantity</th>
                    <th className={Styles.tableHeading}>Allocated Quantity</th>
                    <th className={Styles.tableHeading}>Action</th>
                  </tr>
                </thead>
                <tbody>
                  {purchaseRequestData?.length === 0 ? (
                    <tr>
                      <td colspan="4" style={{ textAlign: 'center' }}>
                        No data found
                      </td>
                    </tr>
                  ) : (
                    purchaseRequestData?.map((item: any, index: any) => {
                      rowIndex = rowIndex + 1;
                      return (
                        <tr>
                          <td>{rowIndex}</td>
                          <td>{item.item_name}</td>
                          <td>{item.requested_quantity}</td>
                          <td>{item.allocated_quantity}</td>
                          <td>
                            <div className={Styles.tablerow}>
                              <DeleteIcon
                                onClick={() => deletePurchaseRequest(index)}
                              />
                            </div>
                          </td>
                        </tr>
                      );
                    })
                  )}
                </tbody>
              </table>
            </div>
          </div>
        </form>
        {/* <div className={Styles.dividerStyle}></div> */}
        <div className={Styles.formButton}>
          <div>
            {/* <Button
              className={Styles.cancelButton}
              shape="rectangle"
              justify="center"
              size="small"
              onClick={() =>
                navigate(`/purchase-detail/${indentId}`, {
                  state: { project_id: projectId },
                })
              }
            >
              Back
            </Button> */}
          </div>
          <div>
            {rowIndex > 0 ? (
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                onClick={handleSubmit}
                icon={<AddIcon color="white" />}
              >
                Raise Purchase Request
              </Button>
            ) : (
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon color="white" />}
                disabled
              >
                Raise Purchase Request
              </Button>
            )}
          </div>
        </div>
      </div>
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
export default PurchaseRequestAdd;
