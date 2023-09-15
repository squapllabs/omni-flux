import React, { useEffect, useState } from 'react';
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
import {
  createIndentRequest,
  updateIndentRequest,
} from '../../../../hooks/indentRequest-hooks';
import IndentRequestService from 'apps/web/src/service/indentRequest-service';
import { format } from 'date-fns';
import * as yup from 'yup';
import PageDisabled from '../../../ui/pageDisableComponent';
import BackArrow from '../../../menu/icons/backArrow';
import { formatBudgetValue } from '../../../../helper/common-function';

const IndentRequest: React.FC = (props: any) => {
  const state: RootState = store.getState();
  let encryptedData = getToken(state, 'Data');
  let userID: number = encryptedData.userId;
  const routeParams = useParams();
  const navigate = useNavigate();
  const [totalCost, setTotalCost] = useState(0);
  const [initialValues, setInitialValues] = useState({
    indent_request_id: '',
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
  const [disabled, setDisabled] = useState(false);
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  useEffect(() => {
    const fetchData = async () => {
      const indentData = await IndentRequestService.getOneIndent(
        Number(routeParams?.indentid)
      );
      console.log('indentData', indentData?.data);
      if (indentData?.data?.approvar_status === 'Rejected') {
        setDisabled(false);
      } else {
        setDisabled(true);
      }
      let obj: any = {
        ...indentData?.data,
        expected_delivery_date: dateFormat(
          indentData?.data?.expected_delivery_date
        ),
      };
      console.log('obj', obj);
      setInitialValues({
        ...indentData?.data,
        expected_delivery_date: dateFormat(
          indentData?.data?.expected_delivery_date
        ),
      });
      setIndentRequestDetailsList(indentData?.data?.indent_request_details);
    };
    if (routeParams?.indentid != undefined) fetchData();
  }, []);
  const priority: any = [
    { value: 'High', label: 'High' },
    { value: 'Medium', label: 'Medium' },
    { value: 'Low', label: 'Low' },
  ];
  const { mutate: postIndentData, isLoading: PostindentLoading } =
    createIndentRequest();
  const { mutate: updateIndentData, isLoading: updateindentLoading } =
    updateIndentRequest();
  const validationSchema = yup.object().shape({
    priority: yup.string().required('Priority is required'),
    expected_delivery_date: yup
      .date()
      .min(new Date(), 'Date must be greater than or equal to the current date')
      .required(' Expected Date is required'),
    description: yup.string().required('Description is required'),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      const sumOfRates = indentRequestDetailsList.reduce(
        (accumulator: any, currentItem: any) => {
          if (currentItem?.is_delete === false)
            return accumulator + currentItem.total;
        },
        0
      );
      formik.values.total_cost = sumOfRates;
      formik.setFieldValue('total_cost', sumOfRates);
      const obj: any = {
        ...values,
        approver_status: 'Pending',
        indent_request_details: indentRequestDetailsList,
      };
      if (routeParams?.indentid != undefined) {
        updateIndentData(obj, {
          onSuccess(data, variables, context) {
            if (data?.status === true) {
              navigate(`/project-edit/${routeParams?.id}`);
            }
          },
        });
      } else {
        postIndentData(obj, {
          onSuccess(data, variables, context) {
            if (data?.status === true) {
              navigate(`/project-edit/${routeParams?.id}`);
            }
          },
        });
      }
    },
  });
  return (
    <div>
      <div className={Styles.indent_container}>
        <div className={Styles.box}>
          <div className={Styles.mainTextContent}>
            <div className={Styles.textContent_1}>
              <h3>Indent Request</h3>
              <span className={Styles.content}>Rise Indent Request</span>
            </div>
            <div className={Styles.backButton}>
              <Button
                type="button"
                color="secondary"
                shape="rectangle"
                size="small"
                justify="center"
                icon={<BackArrow />}
                onClick={(e) => {
                  navigate(`/project-edit/${routeParams?.id}`);
                }}
              >
                Back
              </Button>
            </div>
          </div>
        </div>
        <PageDisabled disabled={disabled}>
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
                        error={
                          formik.touched.priority && formik.errors.priority
                        }
                        disabled={disabled}
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
                        value={formik?.values?.expected_delivery_date}
                        onChange={formik.handleChange}
                        InputProps={{
                          inputProps: {
                            min: `${new Date().toISOString().slice(0, 10)}`,
                          },
                        }}
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
                        value={formatBudgetValue(formik.values.total_cost)}
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
                        error={
                          formik.touched.description &&
                          formik.errors.description
                        }
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
                    disabled={disabled}
                  />
                </div>
                <div
                  style={{
                    display: 'flex',
                    justifyContent: 'flex-end',
                    paddingRight: '50px',
                  }}
                >
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
        </PageDisabled>
      </div>
    </div>
  );
};

export default IndentRequest;
