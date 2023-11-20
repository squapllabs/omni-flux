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
import { useGetAllMachineryForDrop } from '../../../hooks/machinery-hooks';
import NewAddCircleIcon from '../../menu/icons/newAddCircleIcon';

const BomMachinery: React.FC = (props: any) => {
  const navigate = useNavigate();
  const fieldWidth = '100px';
  const DropfieldWidth = '150px';
  let rowIndex = 0;
  const [bomList, setBomList] = useState<any>([]);
  const validationSchema = Yup.object().shape({
    bom_name: Yup.string().trim().required(bomErrorMessages.ENTER_NAME),
    quantity: Yup.number()
      .required(bomErrorMessages.ENTER_QUANTITY)
      .typeError(bomErrorMessages.TYPECHECK),
    machinery_id: Yup.string()
      .trim()
      .required(bomErrorMessages.ENTER_ITEM)
      .test(
        'decimal-validation',
        'Already Exists',
        async function (value, { parent }: Yup.TestContext) {
          const isDelete = parent.is_delete;
          try {
            const isValuePresent = bomList.some((obj: any) => {
              return (
                Number(obj.machinery_id) === Number(value) &&
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
    machinery_id: '',
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
    const type = 'MCNRY';
    const isValuePresent = props.bomList.some((obj: any) => {
      return obj.bom_type === type && obj.is_delete === false;
    });
    if (isValuePresent === false) {
      props.setBomList([...props.bomList, initialValues]);
    }
  });

  useEffect(() => {
    const fetchData = async () => {
      const obj = {
        id: props?.subCategoryId,
        type: props?.activeButton,
      };
      const getData = await BomService.getBOMbySubCatIDandType(obj);
      if (getData?.status === true) {
        setBomList(getData?.data);
        rawMaterialTotalCalulate();
      }
    };
    fetchData();
  }, [reload]);
  const { data: getAllMachineDrop } = useGetAllMachineryForDrop();
  const { data: getAllUomDrop } = useGetUomByType('LABOR');
  const rawMaterialTotalCalulate = async () => {
    const sumOfRates = await bomList.reduce(
      (accumulator: any, currentItem: any) => {
        return accumulator + currentItem.total;
      },
      0
    );
  };

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
    } else if (event.target.name === 'uom_id') {
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
    const tempArry = [...props.bomList];
    tempArry[index] = tempObj;
    props.setBomList(tempArry);
    rawMaterialTotalCalulate();
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
      let arr = [];
      arr = [...props.bomList, values];
      props.setBomList(arr);
      resetForm();
      rawMaterialTotalCalulate();
    },
  });

  const handleAddMachinery = async () => {
    const schema = Yup.array().of(
      Yup.object().shape({
        quantity: Yup.number()
          .required('Quantity is required')
          .typeError('Numbers only allowed')
          .min(1),
        machinery_id: Yup.string()
          .trim()
          .nullable()
          .test(
            'decimal-validation',
            'Already Exists',
            async function (value, { parent }: Yup.TestContext) {
              if (value != null) {
                try {
                  const bOMType = parent.bom_type;
                  if (bOMType === 'MCNRY') {
                    // return true;
                    const dummy: any = [];
                    const allIds = props.bomList.map((item: any) => {
                      // if (item.is_delete === 'N') {
                      //   item.machinery_id;
                      // }
                      if (item.is_delete === false) {
                        dummy.push(item.machinery_id);
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
        uom_id: Yup.string().trim().required('UOM is required'),
        rate: Yup.number()
          .required('Rate is required')
          .typeError('Numbers only allowed')
          .min(1),
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
    setMessage('Machinery detail row has been deleted');
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
                <th>Machine Type</th>
                {/* <th>Description</th> */}
                <th>Rent Type</th>
                <th>Machine Count</th>
                <th>Rate</th>
                <th>Total</th>
                <th>Action</th>
              </tr>
            </thead>
            <tbody>
              {props.bomList?.map((items: any, index: any) => {
                if (items.is_delete === false && items.bom_type === 'MCNRY') {
                  rowIndex = rowIndex + 1;
                  return (
                    <tr>
                      <td>{rowIndex}</td>
                      <td>
                        <AutoCompleteSelect
                          width={DropfieldWidth}
                          name="machinery_id"
                          optionList={
                            getAllMachineDrop != null ? getAllMachineDrop : []
                          }
                          value={items?.machinery_id}
                          onChange={(e) => handleListChange(e, index)}
                          error={
                            props.errors?.[`[${index}].machinery_id`]
                              ? true
                              : false
                          }
                          onSelect={(value) => {
                            const matchingObjects = getAllMachineDrop?.filter(
                              (obj: any) => Number(obj?.value) === Number(value)
                            );
                            let tempObj = {};
                            tempObj = {
                              ...props.bomList[index],
                              machinery_id: value,
                              bom_name: matchingObjects[0]?.label,
                              uom_id: matchingObjects[0]?.data?.uom_id,
                              uom_name:
                                matchingObjects[0]?.data?.uom_data?.name,
                              rate: matchingObjects[0]?.data?.rate,
                            };
                            if (!value) {
                              tempObj.rate = '';
                            }
                            const tempArry = [...props.bomList];
                            tempArry[index] = tempObj;
                            props.setBomList(tempArry);
                          }}
                          addLabel="Add Machinery"
                          onAddClick={(value) => {
                            props.showMachineryForm(true);
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
                          value={items?.quantity}
                          onChange={(e) => handleListChange(e, index)}
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
                          error={
                            props.errors?.[`[${index}].quantity`] ? true : false
                          }
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
            <div onClick={handleAddMachinery} className={Styles.iconContent}>
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

export default BomMachinery;
