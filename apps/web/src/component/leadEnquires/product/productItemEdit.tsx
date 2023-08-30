import { useGetAllItems } from '../../../hooks/item-hooks';
import React, { useState } from 'react';
import * as Yup from 'yup';
import Select from '../../ui/selectNew';
import Input from '../../ui/Input';
import { useFormik } from 'formik';
import Button from '../../ui/Button';
import { getValidateProductyup } from '../../../helper/constants/lead/leadProduct-constants'
import Styles from '../../../styles/leadTender.module.scss';
import CancelIcon from '../../menu/icons/closeIcon';

const ProductItemEdit: React.FC = (props: any) => {
  const validationSchema = getValidateProductyup(Yup);
  const [initialValues, setInitialValues] = useState({
    lead_enquiry_product_item_id:
      props.editProduct.lead_enquiry_product_item_id,
    product_id: `${props.editProduct.product_id}+${props.editProduct.product_name}`,
    quantity: props.editProduct.quantity,
  });
  const { data: getAllItems = [] } = useGetAllItems();

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (values) {
        let productName = values.product_id.split('+');
        let obj = {
          lead_enquiry_product_item_id: values.lead_enquiry_product_item_id,
          product_name: productName[1],
          product_id: Number(productName[0]),
          quantity: Number(values.quantity),
        };
        const itemIndex = props.ProductItems.findIndex(
          (item: any) => item.product_name === obj.product_name
        );
        props.ProductItems[itemIndex] = {
          ...props.ProductItems[itemIndex],
          lead_enquiry_product_item_id: values.lead_enquiry_product_item_id,
          product_name: productName[1],
          product_id: Number(productName[0]),
          quantity: Number(values.quantity),
        };
        props.setProductItems(props.ProductItems);
        props.setOpen(false);
        props.setOpenSnack(true);
        props.setMessage('Product item get updated');
      }
    },
  });
  const handleBack = () => {
    props.setOpen(false);
  };

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.header}>
          <div>
            <h4 className={Styles.titleStyle}>Edit Category</h4>
          </div>
          <div>
            {' '}
            <CancelIcon onClick={handleBack} />
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.field}>
          <Select
            name="product_id"
            label="Product"
            defaultLabel="select a Product"
            value={formik.values.product_id}
            onChange={formik.handleChange}
            error={formik.errors.product_id}
            disabled
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
        <div className={Styles.field}>
          <Input
            label="Quantity"
            name="quantity"
            value={formik.values.quantity}
            onChange={formik.handleChange}
            error={formik.errors.quantity}
          />
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.formButton}>
          <div>
            <Button
              className={Styles.cancelButton}
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleBack}
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
              type="button"
              onClick={formik.handleSubmit}
            >
              Submit
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};

export default ProductItemEdit;
