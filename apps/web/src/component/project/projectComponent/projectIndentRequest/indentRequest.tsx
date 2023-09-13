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
import IndentRequestDetails from './indentRequestDetails';
import { store, RootState } from '../../../../redux/store';
import { getToken } from '../../../../redux/reducer';
import { createIndentRequest } from '../../../../hooks/indentRequest-hooks';

const IndentRequest: React.FC = (props: any) => {
  const state: RootState = store.getState();
  let encryptedData = getToken(state, 'Data');
  let userID: number = encryptedData.userId;
  const routeParams = useParams();
  const navigate = useNavigate();
  const [totalCost, setTotalCost] = useState(0);
  const [initialValues, setInitialValues] = useState({
    requester_user_id: userID,
    priority: '',
    expected_delivery_date: '',
    total_cost: 0,
    description: '',
    created_by: userID,
    requested_date: new Date(),
    project_id: Number(routeParams?.id),
  });

  const [indentRequestDetailsList, setIndentRequestDetailsList] = useState<any>(
    []
  );
  const priority: any = [
    { value: 'High', label: 'High' },
    { value: 'Medium', label: 'Medium' },
    { value: 'Low', label: 'Low' },
  ];
  const { mutate: posIndentData, isLoading: PostindentLoading } =
    createIndentRequest();
  const formik = useFormik({
    initialValues,
    onSubmit: async (values, { resetForm }) => {
      const sumOfRates = indentRequestDetailsList.reduce(
        (accumulator: any, currentItem: any) => {
          return accumulator + currentItem.total;
        },
        0
      );
      formik.values.total_cost = sumOfRates;
      formik.setFieldValue('total_cost', sumOfRates);
      const obj: any = {
        ...values,
        approvar_status: 'Pending',
        indent_request_details: indentRequestDetailsList,
      };
      posIndentData(obj, {
        onSuccess(data, variables, context) {
          if (data?.status === true) {
            navigate(`/project-edit/${routeParams?.id}`);
          }
        },
      });
    },
  });
  return (
    <div>
      <div className={Styles.indent_container}>
        <div className={Styles.box}>
          <div className={Styles.textContent_1}>
            <h3>Indent Request</h3>
            <span className={Styles.content}>Rise Indent Request</span>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.formConatiner}>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.inputFieldMain}>
                <div className={Styles.inputFields}>
                  <div style={{ width: '40%' }}>
                    <Select
                      label="Priority"
                      name="priority"
                      mandatory={true}
                      onChange={formik.handleChange}
                      value={formik.values.priority}
                      defaultLabel="Select from options"
                      placeholder="Select from options"
                      error={formik.touched.priority && formik.errors.priority}
                    >
                      {priority?.map((items: any, index: any) => {
                        return (
                          <option key={items.value} value={items.value}>
                            {items.label}
                          </option>
                        );
                      })}
                    </Select>
                  </div>
                  <div style={{ width: '40%' }}>
                    <DatePicker
                      label="Expected Delivery Date"
                      name="expected_delivery_date"
                      mandatory={true}
                      value={formik.values.expected_delivery_date}
                      onChange={formik.handleChange}
                      error={
                        formik.touched.expected_delivery_date &&
                        formik.errors.expected_delivery_date
                      }
                    />
                  </div>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="Total Cost"
                      name="total_cost"
                      mandatory={true}
                      value={formik.values.total_cost}
                      onChange={formik.handleChange}
                      disabled={true}
                    />
                  </div>
                  <div style={{ width: '40%' }}>
                    <TextArea
                      name="description"
                      label="Indent Description"
                      placeholder="Enter project description"
                      value={formik.values.description}
                      onChange={formik.handleChange}
                      rows={4}
                      maxCharacterCount={400}
                    />
                  </div>
                </div>
              </div>
              <div>
                <IndentRequestDetails
                  projectId={Number(routeParams?.id)}
                  setIndentRequestDetailsList={setIndentRequestDetailsList}
                  indentRequestDetailsList={indentRequestDetailsList}
                  setTotalCost={setTotalCost}
                  totalCost={totalCost}
                />
              </div>
              <div>
                <Button
                  type="button"
                  color="primary"
                  shape="rectangle"
                  size="small"
                  justify="center"
                  onClick={formik.handleSubmit}
                >
                  Save
                </Button>
              </div>
            </form>
          </div>
        </div>
      </div>
    </div>
  );
};

export default IndentRequest;
