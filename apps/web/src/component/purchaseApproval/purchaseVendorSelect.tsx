import React, { useEffect, useState } from 'react';
import { useNavigate, useLocation, useParams } from 'react-router-dom';
import Styles from '../../styles/vendorSelect.module.scss';
import { useFormik } from 'formik';
import CustomSnackBar from '../ui/customSnackBar';
import CustomChip from '../ui/CustomChips';
import { formatBudgetValue } from '../../helper/common-function';
import AddIcon from '../menu/icons/addIcon';
import Button from '../ui/Button';
import * as yup from 'yup';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
// import { useGetAllVendors } from '../../hooks/vendor-hooks';
import { createPurchaseRequest } from '../../hooks/purchaseRequest-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import DeleteIcon from '../menu/icons/newDeleteIcon';
import Input from '../ui/Input';
import CustomDelete from '../ui/customDeleteDialogBox';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { getPurchaseRequestCreateValidateyup } from '../../helper/constants/purchaseRequestAdd-constants';
import VendorService from '../../service/vendor-service';

const VendorChooseModule = () => {
  const [initialValues, setInitialValues] = useState({
    vendor_id: '',
  });
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const navigate = useNavigate();
  const location = useLocation();
  const tableData = location.state.tableData;
  const transformedArray = tableData.map((item: any) => ({
    item_id: item?.bom_detail_data?.item_data?.item_id,
    indent_requested_quantity: item?.indent_requested_quantity,
    purchase_requested_quantity: item?.indent_requested_quantity,
    item_name: item?.bom_detail_data?.item_data.item_name,
    rate: item?.bom_detail_data?.item_data?.rate,
    indent_request_details_id: item?.indent_request_details_id,
    uom_name: item.bom_detail_data?.uom_data?.name,
  }));
  const indentId = location.state.indentId;
  const projectId = location.state.projectId;
  const [tableValue, setTableValue] = useState(transformedArray);
  //   const { data: getAllVendorsData = [], isLoading: dropLoading } =
  //     useGetAllVendors();
  const { mutate: createNewPurchaseRequest } = createPurchaseRequest();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [value, setValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [vendorData, setVendorData] = useState<
    { value: number; label: string }[]
  >([]);
  const [dynamicVendorDropData, setDynamicVendorData] = useState([]);
  const [initialVendorData, setInitialVendorData] = useState([]);
  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    const updatedTableValue = [...tableValue];
    updatedTableValue[index].purchase_requested_quantity = event.target.value;
    setTableValue(updatedTableValue);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const deleteRequestItems = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };

  const deleteItems = (value: any) => {
    tableValue.splice(value, 1);
    setTableValue([...tableValue]);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  const handleDropChange = async () => {
    const vendorData = await VendorService.getAllVendors();
    const arr: any = [];
    const items = vendorData?.data?.map((items: any, index: any) => {
      const obj: any = {
        value: items?.vendor_id,
        label: items?.vendor_name,
      };
      arr.push(obj);
    });
    // console.log('arr', arr);
    setInitialVendorData(arr);
    setDynamicVendorData(arr);
  };

  const deleteVendor = (index: number) => {
    vendorData.splice(index, 1);
    setVendorData([...vendorData]);
    const newArray = initialVendorData.filter((item2) => {
      return !vendorData.some((item1) => item1.label === item2.label);
    });
    setDynamicVendorData(newArray);
  };
  const validationSchema = getPurchaseRequestCreateValidateyup(yup);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const matchingObjects = dynamicVendorDropData?.filter(
        (obj: any) => Number(obj.value) === Number(values.vendor_id)
      );
      setVendorData((prevData) => [...prevData, ...matchingObjects]);
      const newArray = dynamicVendorDropData.filter((item2) => {
        return !matchingObjects.some((item1) => item1.label === item2.label);
      });
      setDynamicVendorData(newArray);
      resetForm();
    },
  });

  const handleSubmit = () => {
    const requestBody = {
      indent_request_id: indentId,
      requester_user_id: userID,
      request_date: new Date(),
      status: 'Waiting For Quotation',
      project_id: projectId,
      purchase_request_details: tableValue.map((item: any) => ({
        item_id: item.item_id,
        indent_requested_quantity: Number(item.indent_requested_quantity),
        purchase_requested_quantity: Number(item.purchase_requested_quantity),
        item_name: item.item_name,
        rate: Number(item?.rate),
        indent_request_details_id: Number(item?.indent_request_details_id),
      })),
      vendor_ids: vendorData.map((data: any) => parseInt(data.value)),
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
  };

  useEffect(() => {
    handleDropChange();
  }, []);
  return (
    <div>
      <div className={Styles.container}>
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
              <h3>Request for Quotation (RFQ)</h3>
              <span className={Styles.content}>
                {' '}
                Raise purchase request against your Project
              </span>
            </div>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        {/* vendor data */}
        {transformedArray?.length !== 0 ? (
          <div>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.mainField}>
                <div className={Styles.leftMainField}>
                  <div className={Styles.leftMainFirst}>
                    <div style={{width:'330px'}}>
                      <AutoCompleteSelect
                        label="Choose Vendor"
                        name="vendor_id"
                        onChange={formik.handleChange}
                        value={formik.values.vendor_id}
                        placeholder="Select from options"
                        mandatory
                        onSelect={(value) => {
                          console.log('value velavan', value);
                          formik.setFieldValue('vendor_id', value);
                        }}
                        addLabel="Add Vendor"
                        onAddClick={(value) => {
                          navigate('/vendor-add', {
                            state: { project_id: projectId, indent_id: indentId },
                          });}}
                        optionList={dynamicVendorDropData}
                        error={
                          formik.touched.vendor_id && formik.errors.vendor_id
                        }
                      />
                    </div>
                    <div>
                      <Button
                        type="submit"
                        color="primary"
                        shape="rectangle"
                        justify="center"
                        size="small"
                        icon={<AddIcon color="white" />}
                      >
                        Add Vendor
                      </Button>
                    </div>
                  </div>
                </div>
                <div className={Styles.dashedLine}></div>
                <div className={Styles.rightMainField}>
                  <div className={Styles.rightHeading}>
                    <span>Selected Vendor for RFQ</span>
                  </div>
                  <div className={Styles.vendorData}>
                    {vendorData.map((item: any, index: any) => (
                      <CustomChip
                        key={item.value}
                        label={item.label}
                        onDelete={() => deleteVendor(index)}
                      />
                    ))}
                  </div>
                </div>
              </div>
            </form>
            {/* table data */}
            <div className={Styles.tableContainer}>
              <div>
                <table className={Styles.scrollable_table}>
                  <thead>
                    <tr>
                      <th className={Styles.tableHeading}>#</th>
                      <th className={Styles.tableHeading}>Item</th>
                      <th className={Styles.tableHeading}>UOM</th>
                      <th className={Styles.tableHeading}>
                        Requested Quantity
                      </th>
                      <th className={Styles.tableHeading}>Unit Price</th>
                      <th className={Styles.tableHeading}>
                        Allocated Quantity
                      </th>
                      <th className={Styles.tableHeading}>Total Price</th>
                      <th className={Styles.tableHeading}>Action</th>
                    </tr>
                  </thead>
                  <tbody>
                    {tableValue?.length === 0 ? (
                      <tr>
                        <td colspan="4" style={{ textAlign: 'center' }}>
                          No data found
                        </td>
                      </tr>
                    ) : (
                      tableValue?.map((item: any, index: any) => {
                        return (
                          <tr>
                            <td>{index + 1}</td>
                            <td>{item?.item_name}</td>
                            <td>{item?.uom_name}</td>
                            <td>{item?.indent_requested_quantity}</td>
                            <td>
                              {item?.rate ? formatBudgetValue(item?.rate) : 0}
                            </td>
                            <td>
                              <div
                                style={{ paddingTop: '7px', height: '40px' }}
                              >
                                <Input
                                  width="100px"
                                  name="purchase_requested_quantity"
                                  mandatory={true}
                                  value={item?.purchase_requested_quantity}
                                  onChange={(e) => handleListChange(e, index)}
                                  //   error={
                                  //     formik.touched
                                  //       .purchase_requested_quantity &&
                                  //     formik.errors.purchase_requested_quantity
                                  //   }
                                  onKeyDown={(e) => {
                                    const isNumber = /^[0-9]*$/.test(e.key);
                                    if (
                                      !isNumber &&
                                      e.key !== 'Backspace' &&
                                      e.key !== 'Delete'
                                    ) {
                                      e.preventDefault();
                                    }
                                  }}
                                />
                              </div>
                            </td>
                            <td>
                              {item?.rate
                                ? formatBudgetValue(
                                    item?.rate *
                                      item?.purchase_requested_quantity
                                  )
                                : 0}
                            </td>
                            <td>
                              <div className={Styles.tablerow}>
                                <DeleteIcon
                                  onClick={() => deleteRequestItems(index)}
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
            <div className={Styles.saveButton}>
              <Button
                type="button"
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon color="white" />}
                disabled={vendorData?.length > 0 ? false : true}
                onClick={() => {
                  handleSubmit();
                }}
              >
                Create PR
              </Button>
            </div>
          </div>
        ) : (
          <div>
            <div className={Styles.emptyDataHandling}>
              <div className={Styles.image}>
                <img src="/stock.jpg" alt="s" width="25%" height="25%" />
              </div>
              <div>
                <h5 className={Styles.textmax}>
                  No items were added to this request.
                </h5>
              </div>
              <div>
                <p className={Styles.textmin}>
                  Add a items for this request by going to the previous menu.
                </p>
              </div>
            </div>
          </div>
        )}
      </div>
      <CustomDelete
        open={openDelete}
        title="Delete Items"
        contentLine1="Are you sure you want to delete this item ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteItems}
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

export default VendorChooseModule;
