import React, { useState, ReactNode } from 'react';
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
    quantity: 0,
    total: 0,
    is_delete: false,
    uom_name: '',
  });
  const [indentDetails, setIndentDetails] = useState<any>();
  const [openDelete, setOpenDelete] = useState(false);
  const bomPostData: any = {
    id: Number(routeParams?.id),
    type: 'RAWMT',
  };
  const { data: getBOMList } = getBOMbyProjectandType(bomPostData);
  const handleIndentDelete = (data: any) => {
    setOpenDelete(true);
    setIndentDetails(data);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const deleteIndentDetail = () => {
    const objectIndex = props.indentRequestDetailsList.findIndex(
      (obj: any) => obj.bom_detail_id === indentDetails?.bom_detail_id
    );
    props.indentRequestDetailsList[objectIndex] = {
      ...props.indentRequestDetailsList[objectIndex],
      is_delete: true,
    };
    props.setIndentRequestDetailsList([...props.indentRequestDetailsList]);
    rowIndex = rowIndex - 1;
    setOpenDelete(false);
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
      total: tempObj?.quantity * matchingObjects[0]?.bom_rate,
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
        async function (value: number, { parent }: yup.TestContext) {
          let isDelete = parent.is_delete;
          try {
            const isValuePresent = props.indentRequestDetailsList.some(
              (obj: any) => {
                return (
                  Number(obj.bom_detail_id) === Number(value) &&
                  obj.is_delete === isDelete
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
    quantity: yup
      .number()
      .moreThan(0, 'Quantity must be more then 0')
      .required('Quantity is required'),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    onSubmit: async (values, { resetForm }) => {
      values['quantity'] = Number(formik?.values?.quantity);
      values['total'] = formik?.values?.quantity * formik?.values?.total;
      await props.setIndentRequestDetailsList([
        ...props.indentRequestDetailsList,
        values,
      ]);
      resetForm();
    },
  });
  return (
    <div>
      <div className={Styles.tableContainer}>
        <div className={Styles.buttons}>
          <Button
            type="button"
            color="primary"
            shape="rectangle"
            size="small"
            justify="center"
            icon={<AddIcon color="white" width={20} />}
            onClick={formik.handleSubmit}
          >
            Add
          </Button>
        </div>
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th className={Styles.tableHeading}>S No</th>
              <th className={Styles.tableHeadingSite}>BOM</th>
              <th className={Styles.tableHeadingSite}>UOM</th>
              <th className={Styles.tableHeading}>Quantity</th>
              <th className={Styles.tableHeading}>Cost</th>
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
                        optionList={getBOMList}
                        disabled
                        value={items?.bom_detail_id}
                      />
                    </td>
                    <td>
                      <Input
                        name="uom_name"
                        mandatory={true}
                        value={items?.uom_name}
                        onChange={formik?.handleChange}
                        disabled={props.disabled}
                      />
                    </td>
                    <td>
                      <Input
                        name="quantity"
                        mandatory={true}
                        value={items?.quantity}
                        onChange={(e) => {
                          handleFieldChange(e, index);
                        }}
                        disabled={props.disabled}
                      />
                    </td>
                    <td>{items.total}</td>
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
                            onClick={(e) => {
                              handleIndentDelete(items);
                            }}
                          />
                        </div>
                      </div>
                    </td>
                  </tr>
                );
              }
            })}
            <tr>
              <td>{rowIndex + 1}</td>
              <td>
                <AutoCompleteSelect
                  name="bom_detail_id"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  mandatory={true}
                  optionList={getBOMList}
                  value={formik.values.bom_detail_id}
                  disabled={props.disabled}
                  onSelect={(value) => {
                    formik.setFieldValue('bom_detail_id', value);
                    const matchingObjects = getBOMList.filter(
                      (obj: any) => Number(obj.value) === Number(value)
                    );

                    formik.setFieldValue(
                      'quantity',
                      matchingObjects[0]?.bom_quantity
                    );
                    console.log(
                      'matchingObjects[0]',
                      matchingObjects[0]?.temp?.uom_data?.name
                    );
                    formik.setFieldValue(
                      'uom_name',
                      matchingObjects[0]?.temp?.uom_data?.name
                    );
                    formik.setFieldValue(
                      'total',
                      matchingObjects[0]?.bom_rate /
                        matchingObjects[0]?.bom_quantity
                    );
                  }}
                  onChange={formik?.handleChange}
                  error={
                    formik.touched.bom_detail_id && formik.errors.bom_detail_id
                  }
                />
              </td>
              <td>
                {' '}
                <Input
                  name="uom_name"
                  mandatory={true}
                  value={formik?.values?.uom_name}
                  onChange={formik?.handleChange}
                  disabled={true}
                />
              </td>
              <td>
                <Input
                  name="quantity"
                  mandatory={true}
                  value={formik?.values?.quantity}
                  onChange={formik?.handleChange}
                  error={formik.touched.quantity && formik.errors.quantity}
                  disabled={props.disabled}
                />
              </td>
              <td>
                <label>
                  {formik?.values?.quantity * formik?.values?.total}
                </label>
              </td>
              <td></td>
            </tr>
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
