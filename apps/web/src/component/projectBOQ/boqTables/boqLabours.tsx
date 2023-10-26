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
import NewAddCircleIcon from '../../menu/icons/newAddCircleIcon';

const BomLabours: React.FC = (props: any) => {
  const navigate = useNavigate();
  const fieldWidth = '100px';
  const DropfieldWidth = '150px';
  let rowIndex = 0;

  // const validationSchema = Yup.object().shape({
  //   // bom_name: Yup.string().trim().required(),
  //   rate: Yup.string().trim().required('Rate is required'),
  //   uom_id: Yup.string().trim().required('UOM is required'),
  //   quantity: Yup.number()
  //     .required(bomErrorMessages.ENTER_QUANTITY)
  //     .typeError(bomErrorMessages.TYPECHECK),
  //   labour_id: Yup.string()
  //     .trim()
  //     .required('Labour is required')
  //     .test(
  //       'decimal-validation',
  //       bomErrorMessages.ITEM_EXIST,
  //       async function (value, { parent }: Yup.TestContext) {
  //         let isDelete = parent.is_delete;
  //         try {
  //           let dummy: any = [];
  //           const allIds = props.bomList.map((item: any) => {
  //             if (item.is_delete === 'N') {
  //               item.labour_id;
  //             }
  //             if (item.is_delete === false) {
  //               dummy.push(item.labour_id);
  //             }
  //           });
  //           const checking = dummy.filter(
  //             (id: any) => Number(id) === Number(value)
  //           ).length;
  //           const isValuePresent = props.bomList.some((obj: any) => {
  //             return (
  //               Number(obj.labour_id) === Number(value) &&
  //               obj.is_delete === false
  //             );
  //           });
  //           if (isValuePresent === false) {
  //             return true;
  //           } else if (checking <= 1) {
  //             return true;
  //           } else return false;
  //         } catch {
  //           return true;
  //         }
  //       }
  //     ),
  // });
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
    if (props.bomList.length === 0 && props.bomId) {
      props.setBomList([...props.bomList, initialValues]);
    }
  }, [props.bomId]);

  const { data: getAllLabourDrop } = useGetAllLabourForDrop();
  const { data: getAllUomDrop } = getUomByType('LABOR');
  const { mutate: bulkBomData, data: responseData } = createBulkBom();

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
      console.log('props', props.bomList[index]);
      tempObj = {
        ...props.bomList[index],
        [event.target.name]: Number(event.target.value),
        total: Number(event.target.value) * props.bomList[index]?.quantity,
      };
      console.log('tempObj', tempObj);
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
    console.log('incoming');
    const schema = Yup.array().of(
      Yup.object().shape({
        // bom_name: Yup.string().trim().required(),
        rate: Yup.string().trim().required('Rate is required'),
        uom_id: Yup.string().trim().required('UOM is required'),
        quantity: Yup.number()
          .required('Quantity is required')
          .typeError('Numbers only allowed'),
        labour_id: Yup.number()
          .nullable()
          .test(
            'decimal-validation',
            'Already Exists',
            async function (value, { parent }: Yup.TestContext) {
              console.log('parent', parent);
              if (value != null) {
                try {
                  const bOMType = parent.bom_type;
                  console.log('bOMType', bOMType);
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
  console.log('error', props.errors);

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
                console.log('props?.bomList', props?.bomList);

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
                          optionList={getAllLabourDrop}
                          value={items?.labour_id}
                          onChange={(e) => handleListChange(e, index)}
                          error={
                            props.errors?.[`[${index}].labour_id`]
                              ? true
                              : false
                          }
                          onSelect={(value) => {
                            const matchingObjects = getAllLabourDrop.filter(
                              (obj: any) => Number(obj.value) === Number(value)
                            );
                            console.log('matchingObjects', matchingObjects);

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
                        <div
                          style={{
                            paddingBottom: '20px',
                          }}
                        >
                          <label> {items?.uom_name}</label>
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

// .test(
//   'decimal-validation',
//   'Already Exists',
//   async function (value, { parent }: Yup.TestContext) {
//     try {
//       const isDelete = parent.is_delete;
//       const bOMType = parent.bom_type;
//       if (bOMType === 'LABOR') {
//         let dummy: any = [];
//         console.log('incoming222');
//         const allIds = props.bomList.map((item: any) => {
//           if (item.is_delete === 'N' && item.bom_type === 'LABOR') {
//             item.labour_id;
//           }
//           if (item.is_delete === false && item.bom_type === 'LABOR') {
//             dummy.push(item.labour_id);
//           }
//         });
//         const checking = dummy.filter(
//           (id: any) => Number(id) === Number(value)
//         ).length;
//         const isValuePresent = props.bomList.some((item: any) => {
//           return (
//             Number(item.labour_id) === Number(value) &&
//             item.is_delete === false &&
//             item.bom_type === 'LABOR'
//           );
//         });
//         if (isValuePresent === false) {
//           return true;
//         } else if (checking <= 1) {
//           return true;
//         } else return false;
//       } else {
//         return true;
//       }
//     } catch {
//       return true;
//     }
//   }
// ),
