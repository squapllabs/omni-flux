import React, { useState, useEffect, ChangeEvent } from 'react';
import Styles from '../../../styles/leadTender.module.scss';
import Input from '../../ui/Input';
import Select from '../../ui/selectNew';
import TextArea from '../../ui/CustomTextArea';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import { useGetAllClient } from 'apps/web/src/hooks/client-hooks';
import { useGetAllUsers } from 'apps/web/src/hooks/user-hooks';
import {
  useGetAllleadEnquiry,
  createleadEnquiry,
  updateleadEnquiry,
} from 'apps/web/src/hooks/leadEnquires-hooks';
import AddIcon from '../../menu/icons/addIcon';
import { getBymasertDataType } from 'apps/web/src/hooks/masertData-hook';
import { useGetAllItems } from 'apps/web/src/hooks/item-hooks';
import DeleteIcon from '../../menu/icons/deleteIcon';
import EditIcon from '../../menu/icons/editIcon';
import {
  getCreateValidateyup,
  getValidateProductyup,
} from 'apps/web/src/helper/constants/lead/leadProduct-constants';
import LeadEnquiresServices from 'apps/web/src/service/leadEnquires-services';
import * as Yup from 'yup';
import { formatBudgetValue } from 'apps/web/src/helper/common-function';
import { environment } from 'apps/web/src/environment/environment';
import CustomSnackBar from '../../ui/customSnackBar';
import { useNavigate } from 'react-router-dom';

interface Item {
  lead_enquiry_product_item_id: string;
  product_id: number;
  product_name: String;
  quantity: number;
}

const ProductSale: React.FC = (props: any) => {
  const navigate = useNavigate();
  const validationSchema = getCreateValidateyup(Yup);
  const valueObject: any = {
    lead_enquiry_product_item_id: '',
    product_id: '',
    quantity: '',
  };

  const [value, setValue] = useState(valueObject);
  const [errors, setErrors] = useState('');
  const [productItems, setProductItems] = useState<Item[]>([]);
  const [appendedValue, setAppendedValue] = useState('');
  const [disable, setDisable] = useState(
    props?.leadEnquireId != undefined ? true : false
  );
  const [openSnack, setOpenSnack] = useState(false);
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
      }[] = [];
      data.data.lead_enquiry_product[0]?.lead_enquiry_product_item.map(
        (item: any) => {
          let Obj = {
            lead_enquiry_product_item_id: item?.lead_enquiry_product_item_id,
            product_name: item?.product?.item_name,
            product_id: item?.product_id,
            quantity: item?.quantity,
          };
          product.push(Obj);
        }
      );
      setProductItems(product);
    };
    if (props.leadEnquireId != undefined) fetchData();
  }, []);
  const { data: getAllClient = [] } = useGetAllClient();
  const { data: getAllUsers = [] } = useGetAllUsers();
  const { data: getClientLevel = [] } = getBymasertDataType('CTLVL');
  const { data: getLeadProbability = [] } = getBymasertDataType('LDPRB');
  const { data: getLeadSource = [] } = getBymasertDataType('LDSE');
  const { data: getAllItems = [] } = useGetAllItems();
  const { mutate: postleadEnquiry } = createleadEnquiry();
  const { mutate: updatelead } = updateleadEnquiry();
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
      product_id: Yup.string()
        .typeError('Product is required')
        .required('Product is required')
        .test(
          'product-availability',
          'Selected product is already present',
          async function (value) {
            let product = value.split('+');
            let productName = product[1];
            try {
              const isValuePresent = productItems.some((obj) => {
                return obj.product_name === productName;
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
    let fileterValue = value.product_name;
    let data = productItems.filter(
      (element) => element?.product_name != fileterValue
    );
    setProductItems(data);
  };
  const handleProductEdit = (e: any, value: any) => {
    let fileterValue = value.product_name;
    let data = productItems.filter(
      (element) => element?.product_name === fileterValue
    );
    // setProductItems(data);
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
      console.log('values', values);
      if (values) {
        console.log('values', values);
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
          console.log('object', object);
          postleadEnquiry(object, {
            onSuccess(data, variables, context) {
              console.log('data', data);
              resetForm;
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
          console.log('editObject', object);
          updatelead(object, {
            onSuccess(data, variables, context) {
              console.log('editData', data);
            },
          });
        }
      }
    },
  });
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
                  disabled={disable}
                />
              </div>
              <div className={Styles.fieldStyle}>
                <Select
                  name="client"
                  label="Client"
                  defaultLabel="select Client"
                  value={formik.values.client}
                  onChange={formik.handleChange}
                  error={formik.touched.client && formik.errors.client}
                  disabled={disable}
                >
                  {getAllClient?.map((option: any) => (
                    <option key={option.client_id} value={option.client_id}>
                      {option.name}
                    </option>
                  ))}
                </Select>
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Select
                  name="source_name"
                  label="Lead Source"
                  defaultLabel="select a Lead Source"
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
                  defaultLabel="select a Client Level"
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
                  defaultLabel="select a Lead Probability"
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
                defaultLabel="select a Product"
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
            <div style={{ paddingTop: '18px' }}>
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
                  {productItems.map((item: any, index: number) => (
                    <tr>
                      <td>{index + 1}</td>
                      <td>{item.product_name}</td>
                      <td>{item.quantity}</td>
                      <td style={{ display: 'flex', gap: '10px' }}>
                        <DeleteIcon
                          onClick={(e: any) => handleProductDelete(e, item)}
                        />
                        <EditIcon
                          onClick={(e: any) => handleProductEdit(e, item)}
                        />
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.fields_container}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <div style={{ display: 'flex', gap: '10px' }}>
                  <Input
                    label="Approx value"
                    name="approx_value"
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
                <Select
                  name="sales_person_name"
                  label="Sales person Name"
                  defaultLabel="select Client"
                  value={formik.values.sales_person_name}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.sales_person_name &&
                    formik.errors.sales_person_name
                  }
                >
                  {getAllUsers?.data?.map((option: any) => (
                    <option key={option.user_id} value={option.user_id}>
                      {option.first_name} {option.last_name}
                    </option>
                  ))}
                </Select>
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <TextArea
                  label="Client Remarks"
                  name="client_remark"
                  value={formik.values.client_remark}
                  onChange={formik.handleChange}
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
                    // type="button"
                    icon={<AddIcon />}
                  >
                    Add Product Sale
                  </Button>
                </div>
              </div>
            </div>
          </div>
        </div>
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </form>
    </div>
  );
};

export default ProductSale;
