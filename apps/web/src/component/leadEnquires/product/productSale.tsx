import React, { useState, useEffect } from 'react';
import Styles from '../../../styles/leadTender.module.scss';
import Input from '../../ui/Input';
import Select from '../../ui/selectNew';
import TextArea from '../../ui/CustomTextArea';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import { useGetAllClient } from 'apps/web/src/hooks/client-hooks';
import { useGetAllUsers } from 'apps/web/src/hooks/user-hooks';
import { useGetAllleadEnquiry } from 'apps/web/src/hooks/leadEnquires-hooks';
import AddIcon from '../../menu/icons/addIcon';
const ProductSale = () => {
  const [selectedValue, setSelectedValue] = useState('');
  const leadType = [
    { value: 'PS', label: 'Product Sale' },
    { value: 'TD', label: 'Tender' },
  ];
  const [initialValues, setInitialValues] = useState({
    lead_type: '',
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
    product_id: '',
    quantity: '',
    created_by: '',
  });

  const { data: getAllClient = [] } = useGetAllClient();
  const { data: getAllUsers = [] } = useGetAllUsers();
  const { data: getAllleadEnquiry = [] } = useGetAllleadEnquiry();
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
  };

  const formik = useFormik({
    initialValues,
    // validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
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
                <Input label="Lead #" />
              </div>
              <div className={Styles.fieldStyle}>
                <Select
                  name="client"
                  label="Client"
                  defaultLabel="select Client"
                  value={formik.values.client}
                  onChange={formik.handleChange}
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
                  name="client"
                  label="Lead Source"
                  defaultLabel="select a Lead Source"
                  onChange={handleDropdownChange}
                  value={selectedValue}
                >
                  {leadType.map((option: any) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
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
                >
                  {leadType.map((option: any) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
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
                >
                  {leadType.map((option: any) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
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
                />
              </div>
            </div>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.container_2}>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Product</th>
                  <th>Quantity</th>
                  <th>Description</th>
                  <th>option</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.fields_container}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Approx value"
                  name="approx_value"
                  value={formik.values.approx_value}
                  onChange={formik.handleChange}
                />
              </div>
              <div className={Styles.fieldStyle}>
                <Select
                  name="sales_person_name"
                  label="Sales person Name"
                  defaultLabel="select Client"
                  value={formik.values.sales_person_name}
                  onChange={formik.handleChange}
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
                />
              </div>
              <div className={Styles.fieldStyle}>
                <TextArea
                  label="Our Remarks"
                  name="our_remarks"
                  value={formik.values.our_remarks}
                  onChange={formik.handleChange}
                />
              </div>
            </div>
            <div className={Styles.button_container}>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                icon={<AddIcon />}
              >
                Add Product Sale
              </Button>
            </div>
          </div>
        </div>
      </form>
    </div>
  );
};

export default ProductSale;
