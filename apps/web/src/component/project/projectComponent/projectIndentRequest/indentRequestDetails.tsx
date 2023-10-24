import React, { useState, ReactNode, useEffect } from 'react';
import Styles from '../../../../styles/project.module.scss';
import { useFormik } from 'formik';
import Input from '../../../ui/Input';
import Select from '../../../ui/selectNew';
import DatePicker from '../../../ui/CustomDatePicker';
import TextArea from '../../../ui/CustomTextArea';
import Button from '../../../ui/Button';
import AutoCompleteSelect from '../../../ui/AutoCompleteSelect';
import { getBOMbyProjectandType } from '../../../../hooks/bom-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import DeleteIcon from '../../../menu/icons/deleteIcon';
import EditIcon from '../../../menu/icons/editIcon';
import CustomDelete from '../../../ui/customDeleteDialogBox';
import * as yup from 'yup';
import { formatBudgetValue } from '../../../../helper/common-function';
import AddIcon from '../../../menu/icons/addIcon';

const IndentRequestDetails: React.FC = (props: any) => {

  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const [initialValues, setInitialValues] = useState({
    indent_request_details_id: '',
    indent_request_id: '',
    bom_detail_id: '',
    indent_requested_quantity: 0,
    total: 0,
    is_delete: false,
    uom_name: '',
    per_item_cost: 0,
  });

  useEffect(() => {
    if (
      props.indentRequestDetailsList.length === 0 &&
      props.indent_id === true
    ) {
      props.setIndentRequestDetailsList([
        ...props.indentRequestDetailsList,
        initialValues,
      ]);
    }
  }, [props.indent_id]);
  const [indentDetails, setIndentDetails] = useState<any>();
  const [indentDetailIndex, setIndentDetailIndex] = useState<any>();
  const [openDelete, setOpenDelete] = useState(false);
  const bomPostData: any = {
    id: Number(routeParams?.id),
    type: 'RAWMT',
  };
  const { data: getBOMList } = getBOMbyProjectandType(bomPostData);

  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  const deleteIndentDetail = (e: any, values: any) => {

    if (
      props.indentRequestDetailsList[indentDetailIndex].bom_detail_id !== ''
    ) {
      props.indentRequestDetailsList[indentDetailIndex] = {
        ...props.indentRequestDetailsList[indentDetailIndex],
        is_delete: true,
      };
    } else {
      props.indentRequestDetailsList.splice(indentDetailIndex, 1);
    }
    props.setIndentRequestDetailsList([...props.indentRequestDetailsList]);
    rowIndex = rowIndex - 1;
    setOpenDelete(false);
    props.setMessage('Indent Request row has been deleted');
    props.setOpenSnack(true);
  };

  const handleFieldChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: number
  ) => {
    let tempObj: any = {};
    tempObj = {
      ...props.indentRequestDetailsList[index],
      [event.target.name]: Number(event.target.value),
    };
    const matchingObjects = getBOMList.filter(
      (obj: any) =>
        Number(obj.value) ===
        Number(props.indentRequestDetailsList[index]?.bom_detail_id)
    );
    tempObj = {
      ...tempObj,
      total: tempObj?.indent_requested_quantity * matchingObjects[0]?.bom_rate,
    };
    const tempArry = [...props.indentRequestDetailsList];
    tempArry[index] = tempObj;

    props.setIndentRequestDetailsList(tempArry);
  };
  const validationSchema = yup.object().shape({
    bom_detail_id: yup
      .string()
      .required('BOM is required')
      .test(
        'decimal-validation',
        'Already exist',
        async function (value, { parent }: yup.TestContext) {
          // let isDelete = parent.is_delete;
          try {
            const isValuePresent = props.indentRequestDetailsList.map(
              (item: any) => {
                return (
                  Number(item.bom_detail_id) === Number(value) &&
                  item.is_delete === false
                );
              }
            );
            if (isValuePresent === false) {
              return true;
            } else return false;
          } catch {
            return true;
          }
        }
      ),
    indent_requested_quantity: yup
      .number()
      .moreThan(0, 'Quantity must be more then 0')
      .required('Quantity is required'),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    onSubmit: async (values, { resetForm }) => {
      values['indent_requested_quantity'] = Number(
        formik?.values?.indent_requested_quantity
      );
      values['total'] =
        formik?.values?.indent_requested_quantity *
        formik?.values?.per_item_cost;
      await props.setIndentRequestDetailsList([
        ...props.indentRequestDetailsList,
        values,
      ]);
      resetForm();
    },
  });

  const handleAddObject = async () => {
    const schema = yup.array().of(
      yup.object().shape({
        bom_detail_id: yup
          .string()
          .required('BOM is required')
          .test(
            'decimal-validation',
            'Already exist',
            async function (value, { parent }: yup.TestContext) {
              const isDelete = false;
              try {
                let dummy: any = [];
                const allIds = props.indentRequestDetailsList.map(
                  (item: any) => {
                    if (item.is_delete === 'N') {
                      item.bom_detail_id;
                    }
                    // console.log('item', item);
                    if (item.is_delete === false)
                      dummy.push(item.bom_detail_id);
                  }
                );
                const isValuePresent = props.indentRequestDetailsList.some(
                  (item: any) => {
                    return (
                      Number(item.bom_detail_id) === Number(value) &&
                      item.is_delete === false
                    );
                  }
                );
                const checking = dummy.filter(
                  (id: any) => Number(id) === Number(value)
                ).length;
                if (checking <= 1) {
                  return true;
                } else if (isValuePresent === false) {
                  return true;
                } else return false;
              } catch {
                return true;
              }
            }
          ),
        indent_requested_quantity: yup
          .number()
          .moreThan(0, 'Quantity must be more then 0')
          .required('Quantity is required'),
      })
    );
    await schema
      .validate(props.indentRequestDetailsList, { abortEarly: false })
      .then(async () => {
        props.setErrors({});
        props.setIndentRequestDetailsList([
          ...props.indentRequestDetailsList,
          initialValues,
        ]);
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

  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    let tempObj = {};
    if (event.target.name !== 'indent_requested_quantity') {
      tempObj = {
        ...props.indentRequestDetailsList[index],
        [event.target.name]: event.target.value,
      };
    }
    let tempArry = [...props.indentRequestDetailsList];
    tempArry[index] = tempObj;
    props.setIndentRequestDetailsList(tempArry);
  };

  return (
    <div>
      <div className={Styles.tableContainerIndent}>
        <div className={Styles.buttons}>
          <Button
            type="button"
            color="primary"
            shape="rectangle"
            size="small"
            justify="center"
            icon={<AddIcon color="white" width={20} />}
            onClick={() => handleAddObject()}
          >
            Add
          </Button>
        </div>
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th className={Styles.tableHeading}>S No</th>
              <th className={Styles.tableHeadingSite}>Item</th>
              <th className={Styles.tableHeadingSite}>UOM</th>
              <th className={Styles.tableHeading}>Quantity</th>
              {/* <th className={Styles.tableHeading}>Per Item Cost</th>
              <th className={Styles.tableHeading}>Total Cost</th> */}
              <th className={Styles.tableHeading}>Action</th>
            </tr>
          </thead>
          <tbody>
            {props.indentRequestDetailsList?.map((items: any, index: any) => {
              if (items?.is_delete === false) {
                rowIndex = rowIndex + 1;
                return (
                  <tr key={index}>
                    <td>{rowIndex}</td>
                    <td>
                      <AutoCompleteSelect
                        name="bom_detail_id"
                        defaultLabel="Select from options"
                        placeholder="Select from options"
                        mandatory={true}
                        optionList={getBOMList != undefined ? getBOMList : []}
                        // optionList={getBOMList}
                        // disabled
                        disabled={props.indent_request_id ? true : false}
                        value={items?.bom_detail_id}
                        onChange={(e) => handleListChange(e, index)}
                        error={
                          props.errors?.[`[${index}].bom_detail_id`]
                            ? true
                            : false
                        }
                        onSelect={(value) => {
                          const matchingObjects = getBOMList.filter(
                            (obj: any) => Number(obj.value) === Number(value)
                          );

                          let tempObj = {};
                          tempObj = {
                            ...props.indentRequestDetailsList[index],
                            bom_detail_id: value,
                            uom_name: matchingObjects[0]?.temp?.uom_data?.name,
                            indent_requested_quantity:
                              matchingObjects[0]?.bom_quantity,
                            total:
                              matchingObjects[0]?.bom_quantity *
                              matchingObjects[0]?.bom_rate,
                          };
                          let tempArry = [...props.indentRequestDetailsList];
                          tempArry[index] = tempObj;
                          props.setIndentRequestDetailsList(tempArry);
                        }}
                      />
                    </td>
                    <td>
                      {/* <Input
                        width="180px"
                        name="uom_name"
                        mandatory={true}
                        value={items?.uom_name}
                        disabled={true}
                        onChange={(e) => handleListChange(e, index)}
                        // error={
                        //   // formik.touched.bom_detail_id && formik.errors.bom_detail_id
                        //   props.errors?.[`[${index}].uom_name`] ? true : false
                        // }
                      /> */}
                      {items?.uom_name}
                    </td>
                    <td>
                      <Input
                        width="180px"
                        name="indent_requested_quantity"
                        // mandatory={true}
                        value={items?.indent_requested_quantity}
                        onChange={(e) => {
                          handleFieldChange(e, index);
                        }}
                        error={
                          props.errors?.[`[${index}].indent_requested_quantity`]
                            ? true
                            : false
                        }
                      />
                    </td>
                    {/* <td>
                      {props.disabled
                        ? items.total / items?.quantity
                        : items.per_item_cost}
                    </td>
                    <td>{items.total}</td> */}
                    <td>
                      <div
                        style={{
                          display: 'flex',
                          justifyContent: 'center',
                          flexDirection: 'row',
                          gap: '20px',
                          alignItems: 'center',
                        }}
                      >
                        <div>
                          <DeleteIcon
                            onClick={() => {
                              setOpenDelete(true);
                              setIndentDetailIndex(index);
                            }}
                          />
                        </div>
                      </div>
                    </td>
                  </tr>
                );
              }
            })}
          </tbody>
        </table>

        <CustomDelete
          open={openDelete}
          title="Delete BOM"
          contentLine1="Are you sure you want to delete this Indent ?"
          contentLine2=""
          handleClose={handleCloseDelete}
          handleConfirm={deleteIndentDetail}
        />
      </div>
    </div>
  );
};

export default IndentRequestDetails;
