import React, { useEffect, useState } from 'react';
import Styles from '../../../../styles/project.module.scss';
import { useFormik } from 'formik';
import Input from '../../../ui/Input';
import Select from '../../../ui/selectNew';
import DatePicker from '../../../ui/CustomDatePicker';
import TextArea from '../../../ui/CustomTextArea';
import Button from '../../../ui/Button';
// import AutoCompleteSelect from '../../../ui/AutoCompleteSelect';
import { getBOMbyProjectandType } from '../../../../hooks/bom-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import IndentRequestDetails from './indentRequestDetails';
import { store, RootState } from '../../../../redux/store';
import { getToken } from '../../../../redux/reducer';
import {
  createIndentRequest,
  updateIndentRequest,
} from '../../../../hooks/indentRequest-hooks';
import IndentRequestService from '../../../../service/indentRequest-service';
import { format } from 'date-fns';
import * as yup from 'yup';
import PageDisabled from '../../../ui/pageDisableComponent';
import { formatBudgetValue } from '../../../../helper/common-function';
import CustomSnackBar from '../../../ui/customSnackBar';
import { getProjectSite } from '../../../../hooks/project-hooks';
import ProjectSubheader from '../../projectSubheader';
import CustomDialogBox from '../../../ui/CustomDialog';

const IndentRequest: React.FC = (props: any) => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
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
    site_id: '',
    request_status: '',
  });
  const [indentRequestDetailsList, setIndentRequestDetailsList] = useState<any>(
    []
  );
  const [errors, setErrors] = useState<any>();
  const [disabled, setDisabled] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openDialog, setOpenDialog] = useState(false);
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const handleCloseDialog = () => {
    setOpenDialog(false);
  };
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
      if (indentData?.data?.approver_status === 'Rejected') {
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
      setInitialValues({
        ...indentData?.data,
        expected_delivery_date: dateFormat(
          indentData?.data?.expected_delivery_date
        ),
      });
      const uomNames = indentData?.data?.indent_request_details?.map(
        (item: any) => item?.bom_detail_data?.uom_data?.name
      );
      const tempArray = indentData?.data?.indent_request_details;
      tempArray.forEach((obj: any, index: number) => {
        obj.uom_name = uomNames[index];
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
  const { data: getAllProjectSiteDatadrop = [] } = getProjectSite(
    Number(routeParams?.id)
  );
  const handleDraft = (e: any) => {
    formik.setFieldValue('request_status', 'Draft');
    formik.submitForm();
  };
  const validationSchema = yup.object().shape({
    priority: yup.string().required('Priority is required'),
    expected_delivery_date: yup
      .date()
      .min(new Date(), 'Date must be greater than or equal to the current date')
      .required(' Expected Date is required'),
    description: yup.string().required('Description is required'),
    site_id: yup.string().required('Site is required'),
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
      let count = 0;
      const schema = yup.array().of(
        yup.object().shape({
          bom_detail_id: yup
            .string()
            .required('BOM is required')
            .test(
              'decimal-validation',
              'Already exist',
              async function (value, { parent }: yup.TestContext) {
                try {
                  let dummy: any = [];
                  const allIds = indentRequestDetailsList.map((item: any) => {
                    if (item.is_delete === 'N') {
                      item.bom_detail_id;
                    }
                    console.log('item', item);
                    if (item.is_delete === false)
                      dummy.push(item.bom_detail_id);
                  });
                  const isValuePresent = indentRequestDetailsList.map(
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
        .validate(indentRequestDetailsList, { abortEarly: false })
        .then(async () => {
          setErrors({});
          for (let i = 0; i < indentRequestDetailsList.length; i++) {
            if (indentRequestDetailsList[i].is_delete === false) {
              count++;
            }
          }
          if (count === 0) {
            setOpenDialog(true);
          } else {
            const obj: any = {
              ...values,
              approver_status: 'Pending',
              indent_request_details: indentRequestDetailsList,
              site_id: Number(formik.values.site_id),
            };

            if (routeParams?.indentid != undefined) {
              updateIndentData(obj, {
                onSuccess(data, variables, context) {
                  if (data?.status === true) {
                    setMessage('Indent Updated successfully');
                    setOpenSnack(true);
                    setTimeout(() => {
                      navigate(`/project-edit/${routeParams?.id}`);
                    }, 2000);
                  }
                },
              });
            } else {
              const obj: any = {
                ...values,
                approver_status: 'Pending',
                indent_request_details: indentRequestDetailsList,
                site_id: Number(formik.values.site_id),
              };
              postIndentData(obj, {
                onSuccess(data, variables, context) {
                  if (data?.status === true) {
                    setMessage('Indent created successfully');
                    setOpenSnack(true);
                    setTimeout(() => {
                      navigate(`/project-edit/${routeParams?.id}`);
                    }, 2000);
                  }
                },
              });
            }
          }
        })
        .catch((e: any) => {
          const errorObj = {};
          e.inner?.map((error: any) => {
            console.log('error', e);
            return (errorObj[error.path] = error.message);
          });
          setErrors({
            ...errorObj,
          });
        });
    },
  });
  return (
    <div>
      <div className={Styles.indent_container}>
        <div className={Styles.box}>
          <ProjectSubheader
            description="Raise Indent Request"
            navigation={`/project-edit/${routeParams?.id}`}
            title="Indent Request"
          />
        </div>
        {/* <PageDisabled disabled={disabled}> */}
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
                    <Select
                      label="Site"
                      name="site_id"
                      mandatory={true}
                      onChange={formik.handleChange}
                      value={formik.values.site_id}
                      defaultLabel="Select from options"
                      placeholder="Select from options"
                      error={formik.touched.site_id && formik.errors.site_id}
                      disabled={disabled}
                    >
                      {getAllProjectSiteDatadrop?.map(
                        (items: any, index: any) => {
                          return (
                            <option key={items.value} value={items.value}>
                              {items.label}
                            </option>
                          );
                        }
                      )}
                    </Select>
                  </div>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="Total Cost"
                      name="total_cost"
                      mandatory={true}
                      value={formatBudgetValue(
                        formik.values.total_cost ? formik.values.total_cost : 0
                      )}
                      onChange={formik.handleChange}
                      disabled={true}
                    />
                  </div>
                </div>
                <div style={{ marginLeft: '2.5%' }}>
                  <div style={{ width: '41%' }}>
                    <TextArea
                      name="description"
                      label="Indent Description"
                      mandatory={true}
                      placeholder="Enter project description"
                      value={formik.values.description}
                      onChange={formik.handleChange}
                      rows={4}
                      maxCharacterCount={400}
                      error={
                        formik.touched.description && formik.errors.description
                      }
                    />
                  </div>
                </div>
              </div>
              <div>
                <IndentRequestDetails
                  projectId={Number(routeParams?.id)}
                  indent_id={routeParams?.indentid === undefined ? true : false}
                  setIndentRequestDetailsList={setIndentRequestDetailsList}
                  indentRequestDetailsList={indentRequestDetailsList}
                  setTotalCost={setTotalCost}
                  totalCost={totalCost}
                  disabled={disabled}
                  errors={errors}
                  setErrors={setErrors}
                  setOpenSnack={setOpenSnack}
                  setMessage={setMessage}
                />
              </div>
              <div
                style={{
                  display: 'flex',
                  justifyContent: 'flex-end',
                  paddingRight: '32px',
                  gap: '20px',
                }}
              >
                <Button
                  type="button"
                  color="secondary"
                  shape="rectangle"
                  size="small"
                  justify="center"
                  onClick={(e) => {
                    handleDraft(e);
                  }}
                >
                  Draft
                </Button>
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
        {/* </PageDisabled> */}
        <CustomDialogBox
          open={openDialog}
          title="Warning"
          contentLine1="Please add indent request details"
          contentLine2=""
          handleClose={handleCloseDialog}
        />
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </div>
    </div>
  );
};

export default IndentRequest;
