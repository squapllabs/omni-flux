import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/bom.module.scss';
import AddIcon from '../../menu/icons/addIcon';
import { useFormik } from 'formik';
import DeleteIcon from '../../menu/icons/deleteIcon';
import Button from '../../ui/Button';
import { createBulkBom } from '../../../hooks/bom-hooks';
import { useGetAllUomDrop, getUomByType } from '../../../hooks/uom-hooks';
import * as Yup from 'yup';
import { useNavigate, useParams } from 'react-router-dom';
import { bomErrorMessages } from '../../../helper/constants/bom-constants';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import Input from '../../ui/Input';
import BomService from '../../../service/bom-service';
import CustomSnackBar from '../../ui/customSnackBar';
import CustomDelete from '../../ui/customDeleteDialogBox';
import { useGetAllLabourForDrop } from '../../../hooks/labour-hooks';

const BomLabours: React.FC = (props: any) => {
  const navigate = useNavigate();
  const fieldWidth = '100px';
  const DropfieldWidth = '150px';
  let rowIndex = 0;
  const validationSchema = Yup.object().shape({
    bom_name: Yup.string().trim().required(bomErrorMessages.ENTER_NAME),
    quantity: Yup.number()
      .required(bomErrorMessages.ENTER_QUANTITY)
      .typeError(bomErrorMessages.TYPECHECK),
    labour_id: Yup.string()
      .trim()
      .required(bomErrorMessages.ENTER_ITEM)
      .test(
        'decimal-validation',
        bomErrorMessages.ITEM_EXIST,
        async function (value: number, { parent }: Yup.TestContext) {
          let isDelete = parent.is_delete;
          try {
            const isValuePresent = props.bomList.some((obj: any) => {
              return (
                Number(obj.labour_id) === Number(value) &&
                obj.is_delete === isDelete
              );
            });
            if (isValuePresent === false) {
              return true;
            } else return false;
          } catch {
            return true;
          }
        }
      ),
    uom_id: Yup.string().trim().required(bomErrorMessages.ENTER_UOM),
  });
  const intialBom: any = {
    created_by: 1,
    sub_category_id: Number(props?.subCategoryId),
    labour_id: '',
    bom_name: '',
    description: '',
    uom_id: '',
    uom_name: '',
    quantity: '',
    rate: '',
    total: 0,
    is_delete: false,
    bom_type: props?.activeButton,
    bom_id: '',
  };
  const [initialValues, setInitialValues] = useState(intialBom);
  const [bomValue, setBomValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [reload, setReload] = useState(false);

  const { data: getAllLabourDrop } = useGetAllLabourForDrop();
  const { data: getAllUomDrop } = getUomByType('LABOR');
  const { mutate: bulkBomData, data: responseData } = createBulkBom();

  const handleDeleteSiteExpense = (e: any, value: any) => {
    setBomValue(value);
    setOpenDelete(true);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    let tempObj = {};
    if (
      event.target.name === 'quantity' ||
      event.target.name === 'price' ||
      event.target.name === 'rate'
    ) {
      tempObj = {
        ...props.bomList[index],
        [event.target.name]: Number(event.target.value),
      };
    } else {
      tempObj = {
        ...props.bomList[index],
        [event.target.name]: event.target.value,
      };
    }

    let tempArry = [...props.bomList];
    tempArry[index] = tempObj;
    props.setBomList(tempArry);
  };
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      values['total'] = formik.values.quantity * formik.values.rate;
      values['is_delete'] = false;
      values['bom_type'] = props?.activeButton;
      values['quantity'] = Number(formik.values.quantity);
      values['rate'] = Number(formik.values.rate);
      values['bom_configuration_id'] = Number(props.bomId);
      console.log('values', values);
      let arr = [];
      arr = [...props.bomList, values];
      props.setBomList(arr);
      resetForm();
    },
  });
  const deleteBOM = () => {
    const itemIndex = props.bomList.findIndex(
      (item: any) =>
        item.labour_id === bomValue?.labour_id &&
        item.is_delete === bomValue?.is_delete
    );
    props.bomList[itemIndex] = {
      ...props.bomList[itemIndex],
      is_delete: true,
    };
    props.setBomList([...props.bomList]);
    rowIndex = rowIndex - 1;
    setOpenDelete(false);
  };
  return (
    <div>
      <div>
        <div className={Styles.tableContainer}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>S No</th>
                <th>Labour Type</th>
                {/* <th>Description</th> */}
                <th>Wages Type</th>
                <th>Labour Count</th>
                <th>Rate</th>
                <th>Total</th>
                <th>Action</th>
              </tr>
            </thead>
            <tbody>
              {props?.bomList?.length != 0
                ? props?.bomList?.map((items: any, index: any) => {
                    if (
                      items.is_delete === false &&
                      items.bom_type === 'LABOR'
                    ) {
                      rowIndex = rowIndex + 1;
                      return (
                        <tr key={index}>
                          <td>{rowIndex}</td>
                          <td style={{ textAlign: 'left' }}>
                            {items.bom_name}
                          </td>
                          {/* <td>
                      <Input
                        name="description"
                        width={fieldWidth}
                        value={items?.description}
                        onChange={(e) => handleListChange(e, index)}
                      />
                    </td> */}
                          <td>
                            <AutoCompleteSelect
                              width={DropfieldWidth}
                              name="uom_id"
                              mandatory={true}
                              optionList={
                                getAllUomDrop != undefined ? getAllUomDrop : []
                              }
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
                              <label>{items.quantity * items.rate}</label>
                            </div>
                          </td>
                          <td>
                            <div
                              style={{
                                cursor: 'pointer',
                                paddingBottom: '20px',
                              }}
                            >
                              <div
                                onClick={(e: any) =>
                                  handleDeleteSiteExpense(e, items)
                                }
                              >
                                <DeleteIcon />
                              </div>
                            </div>
                          </td>
                        </tr>
                      );
                    }
                  })
                : ''}
              <tr>
                <td>{rowIndex + 1}</td>
                <td>
                  <AutoCompleteSelect
                    width={DropfieldWidth}
                    name="labour_id"
                    mandatory={true}
                    optionList={getAllLabourDrop}
                    value={formik.values.labour_id}
                    onChange={formik.handleChange}
                    error={formik.touched.labour_id && formik.errors.labour_id}
                    onSelect={(value) => {
                      formik.setFieldValue('labour_id', value);
                      const matchingObjects = getAllLabourDrop.filter(
                        (obj: any) => Number(obj.value) === Number(value)
                      );
                      formik.setFieldValue(
                        'bom_name',
                        matchingObjects[0]?.label
                      );
                      console.log(
                        'matchingObjects.data?.rate',
                        matchingObjects[0].data?.rate
                      );

                      formik.setFieldValue(
                        'rate',
                        matchingObjects[0].data?.rate
                      );
                    }}
                  />
                </td>
                {/* <td>
                <Input
                  name="description"
                  width={fieldWidth}
                  value={formik.values.description}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.description && formik.errors.description
                  }
                />
              </td> */}
                <td>
                  <AutoCompleteSelect
                    width={DropfieldWidth}
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
        {/* <div className={Styles.saveButton}>
          <Button
            color="primary"
            shape="rectangle"
            justify="center"
            size="small"
            onClick={(e) => handleBulkBomAdd(e)}
          >
            SAVE
          </Button>
        </div> */}
      </div>
      <CustomDelete
        open={openDelete}
        title="Delete BOM"
        contentLine1="Are you sure you want to delete this BOM ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteBOM}
      />
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default BomLabours;
