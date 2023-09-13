import React, { useState } from 'react';
import Styles from '../../../../styles/project.module.scss';
import { useFormik } from 'formik';
import Input from '../../../ui/Input';
import Select from '../../../ui/selectNew';
import DatePicker from '../../../ui/CustomDatePicker';
import TextArea from '../../../ui/CustomTextArea';
import Button from '../../../ui/Button';
import AutoCompleteSelect from '../../../ui/AutoCompleteSelect';
import { getBOMbyProjectandType } from 'apps/web/src/hooks/bom-hooks';
import { useNavigate, useParams } from 'react-router-dom';

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
    is_delete: 'N',
  });
  const bomPostData: any = {
    id: Number(routeParams?.id),
    type: 'RAWMT',
  };
  const { data: getBOMList } = getBOMbyProjectandType(bomPostData);
  const formik = useFormik({
    initialValues,
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
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th className={Styles.tableHeading}>S No</th>
              <th className={Styles.tableHeadingSite}>BOM</th>
              <th className={Styles.tableHeading}>Quantity</th>
              <th className={Styles.tableHeading}>Cost</th>
              <th className={Styles.tableHeading}>Action</th>
            </tr>
          </thead>
          <tbody>
            {props.indentRequestDetailsList?.map((items: any, index: any) => {
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
                  <td>{items.quantity}</td>
                  <td>{items.total}</td>
                </tr>
              );
            })}
            <tr>
              <td></td>
              <td>
                <AutoCompleteSelect
                  name="bom_detail_id"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  mandatory={true}
                  optionList={getBOMList}
                  value={formik.values.bom_detail_id}
                  onSelect={(value) => {
                    formik.setFieldValue('bom_detail_id', value);
                    const matchingObjects = getBOMList.filter(
                      (obj: any) => Number(obj.value) === Number(value)
                    );

                    formik.setFieldValue(
                      'quantity',
                      matchingObjects[0]?.bom_quantity
                    );
                    formik.setFieldValue(
                      'total',
                      matchingObjects[0]?.bom_rate /
                        matchingObjects[0]?.bom_quantity
                    );
                  }}
                  onChange={formik?.handleChange}
                />
              </td>
              <td>
                <Input
                  name="quantity"
                  mandatory={true}
                  value={formik?.values?.quantity}
                  onChange={formik?.handleChange}
                />
              </td>
              {/* <td>
                <Input
                  name="total"
                  mandatory={true}
                  value={formik?.values?.total}
                  onChange={formik?.handleChange}
                />
              </td> */}
              <td>
                <label>
                  {formik?.values?.quantity * formik?.values?.total}
                </label>
              </td>
              <td></td>
            </tr>
          </tbody>
        </table>
        <div className={Styles.buttons}>
          <Button
            type="button"
            color="primary"
            shape="rectangle"
            size="small"
            justify="center"
            onClick={formik.handleSubmit}
          >
            Add
          </Button>
        </div>
      </div>
    </div>
  );
};

export default IndentRequestDetails;
