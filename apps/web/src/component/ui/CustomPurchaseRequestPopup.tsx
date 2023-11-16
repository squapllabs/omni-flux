import React, { useEffect, useState } from 'react';
import Button from '../ui/Button';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import Styles from '../../styles/purchaseRequestPopup.module.scss';
import { useFormik } from 'formik';
import Input from '../ui/Input';
import AutoCompleteSelect from './AutoCompleteSelect';
import AutoCompleteMultiSelect from './AutoCompleteMultiSelect';
import AddIcon from '../menu/icons/addIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import { useGetAllVendors } from '../../hooks/vendor-hooks';
import { useCreatePurchaseRequest } from '../../hooks/purchaseRequest-hooks';
import PurchaseRequestService from '../../service/purchaseRequest-service';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';

const CustomPurchaseRequestPopup = (props: {
  isVissible: any;
  onAction: any;
  indentId: any;
  projectId: any;
  setReload: any;
  setOpenSnack: any;
  setMessage: any;
}) => {
  const {
    isVissible,
    onAction,
    indentId,
    projectId,
    setReload,
    setOpenSnack,
    setMessage,
  } = props;
  const [itemValues, setItemsValues] = useState([]);
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  let rowIndex = 0;
  const [itemsData, setItemsData] = useState();
  const [dropDisable, setDropDisable] = useState(false);
  const [purchaseRequestData, setPurchaseRequestData] = useState<any>([]);
  const [initialValues, setInitialValues] = useState({
    vendor_id: '',
    item_id: '',
    quantity: '',
    item_name: '',
  });

  const { data: getAllVendorsData = [], isLoading: dropLoading } =
    useGetAllVendors();
  const { mutate: createNewPurchaseRequest } = useCreatePurchaseRequest();

  const formik = useFormik({
    initialValues,
    // validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      let arr = [];
      arr = [...purchaseRequestData, values];
      setPurchaseRequestData(arr);
      resetForm();
      setDropDisable(true);
    },
  });

  const handleCloseForm = () => {
    onAction(false);
    setPurchaseRequestData([]);
    setDropDisable(false);
  };

  const handleDropChange = async (obj: any) => {
    const itemsData = await PurchaseRequestService.getProjectItems(projectId);
    const arr: any = [];
    setItemsData(itemsData.data);
    itemsData?.data?.map((items: any, index: any) => {
      const obj: any = {
        value: items?.item_id,
        label: items?.item_data?.item_name,
      };
      arr.push(obj);
    });
    setItemsValues(arr);
  };

  const deletePurchaseRequest = (index: number) => {
    purchaseRequestData.splice(index, 1);
    setPurchaseRequestData([...purchaseRequestData]);
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
        quantity: Number(item.quantity),
        item_name: item.item_name,
      })),
      vendor_ids: purchaseRequestData.reduce(
        (vendorIds: number[], item: any) => {
          const itemVendorIds = Array.isArray(item.vendor_id) // Check if it's an array
            ? item.vendor_id.map((vendor: any) => vendor.value).filter(Boolean) // Filter out empty values
            : [];
          return [...vendorIds, ...itemVendorIds];
        },
        []
      ),
    };

    createNewPurchaseRequest(requestBody, {
      onSuccess: (data, variables, context) => {
        if (data?.message === 'success') {
          setMessage('Purchase Request created');
          setOpenSnack(true);
          setReload(true);
        }
      },
    });

    onAction(false);
    setPurchaseRequestData('');
  };

  useEffect(() => {
    handleDropChange();
  }, [initialValues]);

  return (
    <div>
      <div>
        {isVissible && (
          <CustomPopup>
            <div className={Styles.popupContent}>
              <form onSubmit={formik.handleSubmit}>
                <div className={Styles.header}>
                  <div>
                    <h4>Request for Quotation</h4>
                    <span className={Styles.content}>
                      Raise your purchase request againest your Project
                    </span>
                  </div>
                  <div>
                    <CloseIcon onClick={handleCloseForm} />
                  </div>
                </div>
                <div className={Styles.dividerStyle}></div>
                {/* <div className={Styles.inputFields}> */}
                <div className={Styles.fields_container}>
                  <div className={Styles.fields_container_1}>
                    <div>
                      <AutoCompleteMultiSelect
                        label="Vendors"
                        name="vendor_id"
                        onChange={formik.handleChange}
                        value={formik.values.vendor_id}
                        placeholder="Select from options"
                        mandatory
                        width="350px"
                        onSelect={(value) => {
                          formik.setFieldValue('vendor_id', value);
                        }}
                        optionList={getAllVendorsData}
                        disabled={dropDisable}
                        // error={
                        //   formik.touched.user_id &&
                        //   formik.errors.user_id
                        // }
                      />
                    </div>
                  </div>
                  <div className={Styles.fields_container_2}>
                    <div>
                      <AutoCompleteSelect
                        label="Items"
                        name="item_id"
                        onChange={formik.handleChange}
                        value={formik.values.item_id}
                        placeholder="Select from options"
                        mandatory
                        width="350px"
                        onSelect={(value) => {
                          formik.setFieldValue('item_id', value);

                          const matchingObjects = itemsData?.filter(
                            (obj: any) => Number(obj.item_id) === Number(value)
                          );
                          formik.setFieldValue(
                            'quantity',
                            matchingObjects[0]?.quantity
                          );
                          formik.setFieldValue(
                            'item_name',
                            matchingObjects[0]?.item_data?.item_name
                          );
                        }}
                        optionList={itemValues}
                        // error={
                        //   formik.touched.user_id &&
                        //   formik.errors.user_id
                        // }
                      />
                    </div>
                    <div>
                      <Input
                        label="Quantity"
                        placeholder="Enter Quantity"
                        name="quantity"
                        mandatory={true}
                        width="350px"
                        value={formik.values.quantity}
                        onChange={formik.handleChange}
                        // error={
                        //     formik.touched.quantity && formik.errors.quantity
                        // }
                      />
                    </div>
                    <div>
                      <Button
                        color="primary"
                        shape="rectangle"
                        justify="center"
                        size="small"
                        type="submit"
                        icon={<AddIcon />}
                      >
                        Add
                      </Button>
                    </div>
                  </div>
                </div>
                <div className={Styles.tableContainer}>
                  <div>
                    <table className={Styles.scrollable_table}>
                      <thead>
                        <tr>
                          <th className={Styles.tableHeading}>S NO</th>
                          {/* <th className={Styles.tableHeading}>Vendor Name</th> */}
                          <th className={Styles.tableHeading}>Item</th>
                          <th className={Styles.tableHeading}>Quantity</th>
                          <th className={Styles.tableHeading}>Action</th>
                        </tr>
                      </thead>
                      <tbody>
                        {purchaseRequestData?.length === 0 ? (
                          <tr>
                            <td colspan="5">No data found</td>
                          </tr>
                        ) : (
                          purchaseRequestData?.map((item: any, index: any) => {
                            // let vendorName = item?.vendor_id?.map((vendor: any) => vendor.label).join(', ')
                            rowIndex = rowIndex + 1;
                            return (

                                <tr>
                                  <td>{rowIndex}</td>
                                  {/* <td>{vendorName}</td> */}
                                  <td>{item.item_name}</td>
                                  <td>{item.quantity}</td>
                                  <td>
                                    <div className={Styles.tablerow}>
                                      <DeleteIcon
                                        onClick={() =>
                                          deletePurchaseRequest(index)
                                        }
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
              <div className={Styles.dividerStyle}></div>
              <div className={Styles.formButton}>
                <div>
                  <Button
                    className={Styles.cancelButton}
                    shape="rectangle"
                    justify="center"
                    size="small"
                    onClick={handleCloseForm}
                  >
                    Cancel
                  </Button>
                </div>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    onClick={handleSubmit}
                    icon={<AddIcon />}
                  >
                    Raise Purchase Request
                  </Button>
                </div>
              </div>
            </div>
          </CustomPopup>
        )}
      </div>
    </div>
  );
};
export default CustomPurchaseRequestPopup;
