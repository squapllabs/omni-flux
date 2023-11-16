import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/bom.module.scss';
import AddIcon from '../../menu/icons/addIcon';
import { useFormik } from 'formik';
import DeleteIcon from '../../menu/icons/deleteIcon';
import Button from '../../ui/Button';
import { useCreateBulkBom } from '../../../hooks/bom-hooks';
import { useGetAllUomDrop, useGetUomByType } from '../../../hooks/uom-hooks';
import * as Yup from 'yup';
import { useNavigate, useParams } from 'react-router-dom';
import { bomErrorMessages } from '../../../helper/constants/bom-constants';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import Input from '../../ui/Input';
import BomService from '../../../service/bom-service';
import CustomSnackBar from '../../ui/customSnackBar';
import CustomDelete from '../../ui/customDeleteDialogBox';
import { useGetAllLabourForDrop } from '../../../hooks/labour-hooks';
import NewAddCircleIcon from '../../menu/icons/newAddCircleIcon';

const BomLabours: React.FC = (props: any) => {
  const navigate = useNavigate();
  const fieldWidth = '100px';
  const DropfieldWidth = '150px';
  let rowIndex = 0;
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
    bom_configuration_id: Number(props?.bomId),
  };
  const [initialValues, setInitialValues] = useState(intialBom);
  const [bomValue, setBomValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [reload, setReload] = useState(false);
  const [bomIndex, setBomIndex] = useState<any>();

  useEffect(() => {
    const type = 'LABOR';
    const isValuePresent = props.bomList.some((obj: any) => {
      return obj.bom_type === type && obj.is_delete === false;
    });
    if (isValuePresent === false) {
      props.setBomList([...props.bomList, initialValues]);
    }
  });

  const { data: getAllLabourDrop } = useGetAllLabourForDrop();
  const { data: getAllUomDrop } = useGetUomByType('LABOR');
  const { mutate: bulkBomData, data: responseData } = useCreateBulkBom();

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
    if (event.target.name === 'price' || event.target.name === 'rate') {
      tempObj = {
        ...props.bomList[index],
        [event.target.name]: Number(event.target.value),
        total: Number(event.target.value) * props.bomList[index]?.quantity,
      };
    } else if (event.target.name === 'quantity') {
      tempObj = {
        ...props.bomList[index],
        [event.target.name]: Number(event.target.value),
        total: Number(event.target.value) * props.bomList[index]?.rate,
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

  const handleAddLabour = async () => {
    const schema = Yup.array().of(
      Yup.object().shape({
        rate: Yup.number()
          .required('Rate is required')
          .typeError('Numbers only allowed')
          .min(1),
        uom_id: Yup.string().trim().required('UOM is required'),
        quantity: Yup.number()
          .required('Quantity is required')
          .typeError('Numbers only allowed')
          .min(1),
        labour_id: Yup.string()
          .trim()
          .nullable()
          .test(
            'decimal-validation',
            'Already Exists',
            async function (value, { parent }: Yup.TestContext) {
              if (value != null) {
                try {
                  const bOMType = parent.bom_type;
                  if (bOMType === 'LABOR') {
                    // return true;
                    let dummy: any = [];
                    const allIds = props.bomList.map((item: any) => {
                      if (item.is_delete === 'N') {
                        item.labour_id;
                      }
                      if (item.is_delete === false) {
                        dummy.push(item.labour_id);
                      }
                    });
                    const checking = dummy.filter(
                      (id: any) => Number(id) === Number(value)
                    ).length;
                    if (checking <= 1) {
                      return true;
                    } else return false;
                  } else {
                    return false;
                  }
                } catch {
                  return true;
                }
              } else {
                return true;
              }
            }
          ),
      })
    );
    await schema
      .validate(props.bomList, { abortEarly: false })
      .then(async () => {
        props.setErrors({});
        props.setBomList([...props.bomList, initialValues]);
      })
      .catch((e: any) => {
        const errorObj = {};
        e.inner?.map((error: any) => {
          return (errorObj[error.path] = error.message);
        });
        props.setErrors({
          ...errorObj,
        });
      });
  };

  const deleteBOM = (e: any, values: any) => {
    if (props.bomList[bomIndex].bom_id !== '') {
      props.bomList[bomIndex] = {
        ...props.bomList[bomIndex],
        is_delete: true,
      };
    } else {
      props.bomList.splice(bomIndex, 1);
    }
    props.setBomList([...props.bomList]);
    rowIndex = rowIndex - 1;
    setOpenDelete(false);
    setMessage('Labour detail row has been deleted');
    setOpenSnack(true);
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
              {props?.bomList?.map((items: any, index: any) => {
                if (items.is_delete === false && items.bom_type === 'LABOR') {
                  rowIndex = rowIndex + 1;
                  return (
                    <tr key={index}>
                      <td>{rowIndex}</td>
                      <td>
                        <AutoCompleteSelect
                          width={DropfieldWidth}
                          name="labour_id"
                          mandatory={true}
                          optionList={
                            getAllLabourDrop != null ? getAllLabourDrop : []
                          }
                          value={items?.labour_id}
                          onChange={(e) => handleListChange(e, index)}
                          error={
                            props.errors?.[`[${index}].labour_id`]
                              ? true
                              : false
                          }
                          onSelect={(value) => {
                            const matchingObjects = getAllLabourDrop?.filter(
                              (obj: any) => Number(obj?.value) === Number(value)
                            );
                            let tempObj = {};
                            tempObj = {
                              ...props.bomList[index],
                              labour_id: value,
                              bom_name: matchingObjects[0]?.label,
                              uom_id: matchingObjects[0]?.data?.uom_id,
                              uom_name: matchingObjects[0]?.data?.uom?.name,
                              rate: matchingObjects[0]?.data?.rate,
                            };
                            if (!value) {
                              tempObj.rate = '';
                            }
                            let tempArry = [...props.bomList];
                            tempArry[index] = tempObj;
                            props.setBomList(tempArry);
                          }}
                          addLabel="Add Labour"
                          onAddClick={(value) => {
                            props.showLabourForm(true);
                          }}
                        />
                      </td>
                      <td>
                        <div>
                          <label>
                            {' '}
                            {items?.uom_name
                              ? items?.uom_name
                              : items?.uom_data?.name}
                          </label>
                        </div>
                      </td>
                      <td>
                        <Input
                          width={fieldWidth}
                          name="quantity"
                          mandatory={true}
                          value={items.quantity}
                          onChange={(e) => handleListChange(e, index)}
                          error={
                            props.errors?.[`[${index}].quantity`] ? true : false
                          }
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
                      </td>
                      <td>
                        <Input
                          name="rate"
                          width={fieldWidth}
                          value={items.rate}
                          onChange={(e) => handleListChange(e, index)}
                          error={
                            props.errors?.[`[${index}].rate`] ? true : false
                          }
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
                      </td>
                      <td>
                        <div>
                          <label>{items.quantity * items.rate}</label>
                        </div>
                      </td>
                      <td>
                        <div
                          style={{
                            cursor: 'pointer',
                          }}
                        >
                          <div
                            onClick={() => {
                              setOpenDelete(true);
                              setBomIndex(index);
                            }}
                          >
                            <DeleteIcon />
                          </div>
                        </div>
                      </td>
                    </tr>
                  );
                }
              })}
            </tbody>
          </table>
          <div className={Styles.addDataIcon}>
            <div onClick={handleAddLabour} className={Styles.iconContent}>
              <NewAddCircleIcon />
              <span>Add Plan here</span>
            </div>
          </div>
        </div>
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
