import React, { useState, useEffect, ChangeEvent } from 'react';
import Styles from '../../../styles/leadTender.module.scss';
import Input from '../../ui/Input';
import Select from '../../ui/selectNew';
import TextArea from '../../ui/CustomTextArea';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import {
  useGetAllClient,
  useGetAllClientDrop,
} from 'apps/web/src/hooks/client-hooks';
import {
  useGetAllUsers,
  useGetAllUsersDrop,
} from 'apps/web/src/hooks/user-hooks';
import {
  createleadEnquiry,
  updateleadEnquiry,
} from 'apps/web/src/hooks/leadEnquires-hooks';
import AddIcon from '../../menu/icons/addIcon';
import { getBymasertDataType } from 'apps/web/src/hooks/masertData-hook';
import {
  useGetAllItems,
  useGetAllItemsDrops,
} from 'apps/web/src/hooks/item-hooks';
import DeleteIcon from '../../menu/icons/deleteIcon';
import EditIcon from '../../menu/icons/editIcon';
import { getCreateValidateyup } from 'apps/web/src/helper/constants/lead/leadProduct-constants';
import LeadEnquiresServices from 'apps/web/src/service/leadEnquires-services';
import * as Yup from 'yup';
import { formatBudgetValue } from 'apps/web/src/helper/common-function';
import { environment } from 'apps/web/src/environment/environment';
import CustomSnackBar from '../../ui/customSnackBar';
import { useNavigate } from 'react-router-dom';
import CustomEditDialog from '../../../component/ui/customEditDialogBox';
import ProductItemEdit from './productItemEdit';
import CustomDelete from '../../ui/customDeleteDialogBox';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';

interface Item {
  lead_enquiry_product_item_id: string;
  product_id: number;
  product_name: String;
  quantity: number;
  is_delete: string;
}

const ProductSale: React.FC = (props: any) => {
  const navigate = useNavigate();
  const validationSchema = getCreateValidateyup(Yup);
  const valueObject: any = {
    lead_enquiry_product_item_id: '',
    product_id: '',
    quantity: '',
    is_delete: 'N',
  };
  let rowIndex = 0;

  const [value, setValue] = useState(valueObject);
  const [errors, setErrors] = useState('');
  const [productItems, setProductItems] = useState<Item[]>([]);
  const [editProduct, setEditProduct] = useState();
  const [appendedValue, setAppendedValue] = useState('');
  const [disable, setDisable] = useState(
    props?.leadEnquireId != undefined ? true : false
  );
  const [openDelete, setOpenDelete] = useState(false);
  const [open, setOpen] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [count, setCount] = useState(0);
  const [initialValues, setInitialValues] = useState({
    lead_code: '',
    lead_enquiry_id: '',
    lead_type: props.leadType,
    client: '',
    client_level: '',
    client_contact_name: '',
    client_contact_email: '',
    client_contact_phone: '',
    our_remarks: '',
    client_remark: '',
    doc_url: '',
    status_remarks: '',
    source_name: '',
    status: '',
    probability: '',
    approx_value: '',
    sales_person_name: '',
    created_by: '',
    lead_product_id: '',
  });
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const deleteProductItem = () => {
    const itemIndex = productItems.findIndex(
      (item: any) =>
        item.product_name === editProduct?.product_name &&
        item.is_delete === editProduct?.is_delete
    );
    productItems[itemIndex] = {
      ...productItems[itemIndex],
      is_delete: 'Y',
    };
    setProductItems([...productItems]);
    rowIndex = rowIndex - 1;
    handleCloseDelete();
    setMessage('Product item Successfully deleted');
    setOpenSnack(true);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  useEffect(() => {
    const fetchData = async () => {
      let data = await LeadEnquiresServices.getOneleadEnquiryByID(
        props.leadEnquireId
      );

      setInitialValues({
        lead_enquiry_id: props?.leadEnquireId,
        lead_type: props.leadType,
        lead_code: data?.data?.lead_code,
        client: data?.data?.client,
        client_level: data?.data?.client_level,
        client_contact_name: data?.data?.client_contact_name,
        client_contact_email: data?.data?.client_contact_email,
        client_contact_phone: data?.data?.client_contact_phone,
        our_remarks: data?.data?.our_remarks,
        client_remark: data?.data?.client_remark,
        doc_url: data?.data?.doc_url,
        status_remarks: data?.data?.status_remarks,
        source_name: data?.data?.lead_enquiry_product[0]?.source_name,
        status: data?.data?.status,
        probability: data?.data?.lead_enquiry_product[0]?.probability,
        approx_value: data?.data?.lead_enquiry_product[0]?.approx_value,
        sales_person_name:
          data?.data?.lead_enquiry_product[0]?.sales_person_name,
        created_by: data?.data?.created_by,
        lead_product_id: data?.data?.lead_enquiry_product[0]?.lead_product_id,
      });
      let product: {
        product_name: any;
        product_id: any;
        quantity: any;
        lead_enquiry_product_item_id: any;
        is_delete: any;
      }[] = [];
      data.data.lead_enquiry_product[0]?.lead_enquiry_product_item.map(
        (item: any) => {
          let Obj = {
            lead_enquiry_product_item_id: item?.lead_enquiry_product_item_id,
            product_name: item?.product?.item_name,
            product_id: item?.product_id,
            quantity: item?.quantity,
            is_delete: 'N',
          };
          product.push(Obj);
        }
      );
      setProductItems(product);
    };
    if (props.leadEnquireId != undefined) fetchData();
    if (props.leadEnquireId === undefined) fetchLeadID();
  }, []);
  const { data: getAllClient = [] } = useGetAllClientDrop();
  const { data: getAllUsers = [] } = useGetAllUsersDrop();
  const { data: getClientLevel = [] } = getBymasertDataType('CTLVL');
  const { data: getLeadProbability = [] } = getBymasertDataType('LDPRB');
  const { data: getLeadSource = [] } = getBymasertDataType('LDSE');
  const { data: getAllItems = [] } = useGetAllItems();

  const { mutate: postleadEnquiry } = createleadEnquiry();
  const { mutate: updatelead } = updateleadEnquiry();
  const fetchLeadID = async () => {
    const leadID = await LeadEnquiresServices.getLeadID(props.leadType);
    initialValues.lead_code = leadID?.data;
    setInitialValues({ ...initialValues });
  };
  const handleChangeItems = (
    event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
  ) => {
    setValue({
      ...value,
      [event.target.name]: event.target.value,
    });
  };
  const handleAddItems = async () => {
    setErrors({});
    const schema = Yup.object().shape({
      is_delete: Yup.string().required(),
      product_id: Yup.string()
        .typeError('Product is required')
        .required('Product is required')
        .test(
          'product-availability',
          'Selected product is already present',
          async function (value, { parent }: Yup.TestContext) {
            let isDelete = parent.is_delete;
            let product = value.split('+');
            let productName = product[1];
            try {
              const isValuePresent = productItems.some((obj) => {
                return (
                  obj.product_name === productName && obj.is_delete === isDelete
                );
              });
              if (isValuePresent === true) {
                return false;
              } else {
                return true;
              }
            } catch {
              return true;
            }
          }
        ),
      quantity: Yup.number()
        .typeError('Quantity is required')
        .required('Quantity is required'),
    });
    await schema
      .validate(value, { abortEarly: false })
      .then(async () => {
        let productName = value.product_id.split('+');
        value.product_id = Number(productName[0]);
        value.product_name = productName[1];
        value.quantity = Number(value.quantity);
        value.is_delete = 'N';
        setProductItems([...productItems, value]);
        setValue(valueObject);
      })
      .catch((e) => {
        let errorObj: any = {};
        e.inner.map((errors: any) => {
          return (errorObj[errors.path] = errors.message);
        });
        setErrors({
          ...errorObj,
        });
      });
  };
  const handleProductDelete = (e: any, value: any) => {
    setEditProduct(value);
    setOpenDelete(true);
  };
  const handleProductEdit = (e: any, value: any) => {
    setOpen(true);
    let fileterValue = value.product_name;
    let data = productItems.filter(
      (element) => element?.product_name === fileterValue
    );
    setEditProduct(data[0]);
  };

  const handleValueChange = (event: any) => {
    const budgetValue = event.target.value;
    const data = formatBudgetValue(Number(budgetValue));
    setAppendedValue(data);
    formik.setFieldValue('budget', budgetValue);
    formik.handleChange(event);
  };

  const outputLableNameFromEnv = `Approx value (${environment.OUTPUTBUDGET})`;

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        if (props?.leadEnquireId === undefined) {
          let object: any = {
            lead_type: props.leadType,
            client: Number(values.client),
            client_level: Number(values.client_level),
            client_contact_name: values.client_contact_name,
            client_contact_email: values.client_contact_email,
            client_contact_phone: values.client_contact_phone,
            our_remarks: values.our_remarks,
            client_remark: values.client_remark,
            doc_url: '',
            status_remarks: '',
            source_name: values.source_name,
            status: '',
            probability: Number(values.probability),
            approx_value: Number(values.approx_value),
            sales_person_name: Number(values.sales_person_name),
            created_by: 56,
            product_item: productItems,
          };
          postleadEnquiry(object, {
            onSuccess(data, variables, context) {
              resetForm;
              setMessage('Product sale created');
              setOpenSnack(true);
              setInterval(() => {
                navigate('/lead-enquires');
              }, 3000);
            },
          });
        } else {
          let object: any = {
            lead_enquiry_id: Number(props.leadEnquireId),
            lead_type: props.leadType,
            lead_product_id: Number(values.lead_product_id),
            client: Number(values.client),
            client_level: Number(values.client_level),
            client_contact_name: values.client_contact_name,
            client_contact_email: values.client_contact_email,
            client_contact_phone: values.client_contact_phone,
            our_remarks: values.our_remarks,
            client_remark: values.client_remark,
            doc_url: '',
            status_remarks: '',
            source_name: values.source_name,
            status: '',
            probability: Number(values.probability),
            approx_value: Number(values.approx_value),
            sales_person_name: Number(values.sales_person_name),
            created_by: 56,
            product_item: productItems,
          };
          updatelead(object, {
            onSuccess(data, variables, context) {
              setMessage('Product sale edited');
              setOpenSnack(true);
              setInterval(() => {
                navigate('/lead-enquires');
              }, 3000);
            },
          });
        }
      }
    },
  });
  const handleClose = () => {
    setOpen(false);
  };
  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.box}>
          <div className={Styles.fields_container}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Lead #"
                  name="lead_code"
                  value={formik.values.lead_code}
                  onChange={formik.handleChange}
                  disabled
                />
              </div>
              <div className={Styles.fieldStyle}>
                <AutoCompleteSelect
                  name="client"
                  label="Client"
                  defaultLabel="Select Client"
                  mandatory={true}
                  value={formik.values.client}
                  onChange={formik.handleChange}
                  error={formik.touched.client && formik.errors.client}
                  onSelect={(value) => {
                    formik.setFieldValue('client', value);
                  }}
                  disabled={disable}
                  optionList={getAllClient}
                />
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Select
                  name="source_name"
                  label="Lead Source"
                  defaultLabel="Select a Lead Source"
                  mandatory={true}
                  onChange={formik.handleChange}
                  value={formik.values.source_name}
                  error={
                    formik.touched.source_name && formik.errors.source_name
                  }
                >
                  {getLeadSource?.map((option: any) => (
                    <option
                      key={option.master_data_name}
                      value={option.master_data_name}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
              <div className={Styles.fieldStyle}>
                <Select
                  name="client_level"
                  label="Client Level"
                  defaultLabel="Select a Client Level"
                  mandatory={true}
                  value={formik.values.client_level}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.client_level && formik.errors.client_level
                  }
                >
                  {getClientLevel?.map((option: any) => (
                    <option
                      key={option.master_data_id}
                      value={option.master_data_id}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                {' '}
                <Select
                  name="probability"
                  label="Lead Probability"
                  defaultLabel="Select a Lead Probability"
                  mandatory={true}
                  value={formik.values.probability}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.probability && formik.errors.probability
                  }
                >
                  {getLeadProbability?.map((option: any) => (
                    <option
                      key={option.master_data_id}
                      value={option.master_data_id}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Client Contact Name"
                  name="client_contact_name"
                  mandatory={true}
                  value={formik.values.client_contact_name}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.client_contact_name &&
                    formik.errors.client_contact_name
                  }
                />
              </div>
            </div>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.fields_container_1}>
            <div className={Styles.FieldItemStyle}>
              
              <Select
                name="product_id"
                label="Product"
                defaultLabel="Select a Product"
                value={value?.product_id}
                onChange={handleChangeItems}
                error={errors?.product_id}
              >
                {getAllItems?.map((option: any) => (
                  <option
                    key={option.item_id}
                    value={`${option.item_id}+${option.item_name}`}
                  >
                    {option.item_name}
                  </option>
                ))}
              </Select>
            </div>
            <div className={Styles.FieldItemStyle}>
              <Input
                label="Quantity"
                name="quantity"
                value={value?.quantity}
                onChange={handleChangeItems}
                error={errors?.quantity}
              />
            </div>
            <div className={Styles.itemAdd}>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                icon={<AddIcon width={20} />}
                type="button"
                onClick={handleAddItems}
              >
                Add
              </Button>
            </div>
          </div>
          <div className={Styles.fields_container_1}>
            <div className={Styles.container_2}>
              <table>
                <thead>
                  <tr>
                    <th>S No</th>
                    <th>Product</th>
                    <th>Quantity</th>
                    <th>Option</th>
                  </tr>
                </thead>
                <tbody>
                  {productItems.length === 0 ? (
                    <tr>
                      <td></td>
                      <td></td>
                      <td>No Data</td>
                      <td></td>
                    </tr>
                  ) : (
                    ''
                  )}
                  {productItems.map((item: any, index: number) => {
                    if (item?.is_delete === 'N') {
                      rowIndex = rowIndex + 1;
                      return (
                        <>
                          <tr key={index}>
                            <td>{rowIndex}</td>
                            <td>{item.product_name}</td>
                            <td>{item.quantity}</td>
                            <td className={Styles.tableData}>
                              <DeleteIcon
                                onClick={(e: any) =>
                                  handleProductDelete(e, item)
                                }
                              />
                              <EditIcon
                                onClick={(e: any) => handleProductEdit(e, item)}
                              />
                            </td>
                          </tr>
                        </>
                      );
                    }
                  })}
                </tbody>
              </table>
            </div>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.fields_container}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <div className={Styles.tableData}>
                  <Input
                    label="Approx value"
                    name="approx_value"
                    mandatory={true}
                    value={formik.values.approx_value}
                    onChange={handleValueChange}
                    error={
                      formik.touched.approx_value && formik.errors.approx_value
                    }
                  />
                  <Input
                    name="label_field"
                    label={outputLableNameFromEnv}
                    value={appendedValue}
                  />
                </div>
              </div>
              <div className={Styles.fieldStyle}>
                <AutoCompleteSelect
                  name="sales_person_name"
                  label="Sales person Name"
                  defaultLabel="Select Sales Person"
                  mandatory={true}
                  value={formik.values.sales_person_name}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.sales_person_name &&
                    formik.errors.sales_person_name
                  }
                  onSelect={(value) => {
                    formik.setFieldValue('sales_person_name', value);
                  }}
                  optionList={getAllUsers}
                />
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <TextArea
                  label="Client Remarks"
                  name="client_remark"
                  value={formik.values.client_remark}
                  onChange={formik.handleChange}
                  maxCharacterCount={200}
                  error={
                    formik.touched.client_remark && formik.errors.client_remark
                  }
                />
              </div>
              <div className={Styles.fieldStyle}>
                <TextArea
                  label="Our Remarks"
                  name="our_remarks"
                  value={formik.values.our_remarks}
                  onChange={formik.handleChange}
                  maxCharacterCount={200}
                  error={
                    formik.touched.our_remarks && formik.errors.our_remarks
                  }
                />
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}></div>
              <div className={Styles.fieldStyle}>
                <div className={Styles.button_container}>
                  <Button
                    shape="rectangle"
                    justify="center"
                    size="small"
                    color="primary"
                  >
                    Submit
                  </Button>
                </div>
              </div>
            </div>
          </div>
        </div>
        <CustomEditDialog
          open={open}
          title="Edit Product Item"
          handleClose={handleClose}
          content={
            <ProductItemEdit
              setOpen={setOpen}
              open={open}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
              setProductItems={setProductItems}
              ProductItems={productItems}
              setEditproduct={setEditProduct}
              editProduct={editProduct}
            />
          }
        />
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
        <CustomDelete
          open={openDelete}
          title="Delete Product Item"
          contentLine1="Are you sure you want to delete this Product ?"
          contentLine2=""
          handleClose={handleCloseDelete}
          handleConfirm={deleteProductItem}
        />
      </form>
    </div>
  );
};

export default ProductSale;
