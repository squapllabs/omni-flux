import React, { useState } from 'react';
import { useGetAllItemsDrops } from '../../hooks/item-hooks';
import { useGetAllUomDrop } from '../../hooks/uom-hooks';
import Styles from '../../styles/bom.module.scss';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Input from '../ui/Input';
import AddIcon from '../menu/icons/addIcon';
import { useFormik } from 'formik';
import DeleteIcon from '../menu/icons/deleteIcon';

const Bom: React.FC = (props: any) => {
  const fieldWidth = '80px';
  const intialBom: any = {
    created_by: 1,
    sub_category_id: props?.selectedSubCategory,
    item_id: '',
    bom_name: '',
    description: '',
    uom_id: '',
    uom_name: '',
    quantity: '',
    rate: '',
    total: 0,
    is_delete: false,
  };
  const [initialValues, setInitialValues] = useState(intialBom);
  const [bomList, setBomList] = useState<any>([]);
  const { data: getAllItemDrop } = useGetAllItemsDrops();
  const { data: getAllUomDrop } = useGetAllUomDrop();
  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    console.log('bomData', bomList[index]);
    let tempObj = {};
    tempObj = {
      ...bomList[index],
      [event.target.name]: event.target.value,
    };
    let tempArry = [...bomList];
    tempArry[index] = tempObj;
    setBomList(tempArry);
  };
  const formik = useFormik({
    initialValues,
    // validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      values['total'] = formik.values.quantity * formik.values.rate;
      values['is_delete'] = false;
      console.log('values', values);
      let arr = [];
      arr = [...bomList, values];
      setBomList(arr);
      resetForm;
    },
  });
  return (
    <div>
      <div className={Styles.tableContainer}>
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th>S No</th>
              <th>Item</th>
              <th>Description</th>
              <th>UOM</th>
              <th>Quantity</th>
              <th>Rate</th>
              <th>Total</th>
              <th>Action</th>
            </tr>
          </thead>
          <tbody>
            {bomList?.map((items: any, index: any) => {
              if (items.is_delete === false) {
                return (
                  <tr>
                    <td>1</td>
                    <td>{items.bom_name}</td>
                    <td>
                      <Input
                        name="description"
                        width={fieldWidth}
                        value={items?.description}
                        onChange={(e) => handleListChange(e, index)}
                      />
                    </td>
                    <td>
                      <AutoCompleteSelect
                        width="200px"
                        name="uom_id"
                        mandatory={true}
                        optionList={getAllUomDrop}
                        value={items.uom_id}
                        onChange={(e) => handleListChange(e, index)}
                      />
                    </td>
                    <td>
                      <Input
                        width={fieldWidth}
                        name="quantity"
                        mandatory={true}
                        value={items.quantity}
                        onChange={(e) => handleListChange(e, index)}
                      />
                    </td>
                    <td>
                      <Input
                        name="rate"
                        width={fieldWidth}
                        value={items.rate}
                        onChange={(e) => handleListChange(e, index)}
                      />
                    </td>
                    <td>
                      <div
                        style={{
                          paddingBottom: '20px',
                        }}
                      >
                        <label>{items.total}</label>
                      </div>
                    </td>
                    <td>
                      <div
                        style={{
                          cursor: 'pointer',
                          paddingBottom: '20px',
                        }}
                      >
                        <DeleteIcon />
                      </div>
                    </td>
                  </tr>
                );
              }
            })}
            <tr>
              <td>1</td>
              <td>
                <AutoCompleteSelect
                  width="120px"
                  name="item_id"
                  mandatory={true}
                  optionList={getAllItemDrop}
                  value={formik.values.item_id}
                  onChange={formik.handleChange}
                  error={formik.touched.item_id && formik.errors.item_id}
                  onSelect={(value) => {
                    formik.setFieldValue('item_id', value);
                    const matchingObjects = getAllItemDrop.filter(
                      (obj: any) => Number(obj.value) === Number(value)
                    );
                    formik.setFieldValue('bom_name', matchingObjects[0]?.label);
                  }}
                />
              </td>
              <td>
                <Input
                  name="description"
                  width={fieldWidth}
                  value={formik.values.description}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.description && formik.errors.description
                  }
                />
              </td>
              <td>
                <AutoCompleteSelect
                  width="200px"
                  name="uom_id"
                  mandatory={true}
                  optionList={getAllUomDrop}
                  value={formik.values.uom_id}
                  onChange={formik.handleChange}
                  error={formik.touched.uom_id && formik.errors.uom_id}
                  onSelect={(value) => {
                    formik.setFieldValue('uom_id', value);
                  }}
                />
              </td>
              <td>
                <Input
                  width={fieldWidth}
                  name="quantity"
                  mandatory={true}
                  value={formik.values.quantity}
                  onChange={formik.handleChange}
                  error={formik.touched.quantity && formik.errors.quantity}
                />
              </td>
              <td>
                <Input
                  name="rate"
                  width={fieldWidth}
                  value={formik.values.rate}
                  onChange={formik.handleChange}
                  error={formik.touched.rate && formik.errors.rate}
                />
              </td>
              <td>
                <label>{formik.values.quantity * formik.values.rate}</label>
              </td>
              <td>
                <div
                  style={{
                    cursor: 'pointer',
                    paddingBottom: '20px',
                  }}
                >
                  <div onClick={formik.handleSubmit}>
                    <AddIcon />
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
  );
};

export default Bom;
