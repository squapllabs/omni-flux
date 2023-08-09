import { useGetAllItems } from 'apps/web/src/hooks/item-hooks';
import React, { useState } from 'react';
import * as Yup from 'yup';
import Select from '../../ui/selectNew';
import Input from '../../ui/Input';
import { useFormik } from 'formik';
import Button from '../../ui/Button';
import { getValidateProductyup } from 'apps/web/src/helper/constants/lead/leadProduct-constants';

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
    <div>
      <form onSubmit={formik.handleSubmit}>
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
        <Input
          label="Quantity"
          name="quantity"
          value={formik.values.quantity}
          onChange={formik.handleChange}
          error={formik.errors.quantity}
        />
        <div
          style={{ display: 'flex', gap: '10px', justifyContent: 'flex-end' }}
        >
          <div>
            <Button
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
