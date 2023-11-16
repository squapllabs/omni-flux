import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/machinery.module.scss';
import { useGetByUomType } from '../../hooks/uom-hooks';
import { useNavigate } from 'react-router-dom';
import DatePicker from '../ui/CustomDatePicker';
import { getCreateValidateyup } from '../../helper/constants/machinery-constants';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import { useCreateMachinery, useUpdateMachinery } from '../../hooks/machinery-hooks';
import CustomSnackBar from '../ui/customSnackBar';
import { useParams } from 'react-router-dom';
import machineryService from '../../service/machinery-service';
import { format } from 'date-fns';
import Select from '../ui/selectNew';
import ProjectSubheader from '../project/projectSubheader';

const AddMachinery = () => {
  const { data: getAllUomList = [] } = useGetByUomType();
  const { mutate: createNewMachinery } = useCreateMachinery();
  const { mutate: updateOneMachinery } = useUpdateMachinery();
  const routeParams = useParams();
  const navigate = useNavigate();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userData: any = encryptedData.userData;
  const validationSchema = getCreateValidateyup(Yup);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');

  const dateFormat = (value: any) => {
    if (value !== null) {
      const currentDate = new Date(value);
      const formattedDate = format(currentDate, 'yyyy-MM-dd');
      return formattedDate;
    }
  };
  const [initialValues, setInitialValues] = useState({
    machinery_name: '',
    machinery_type: '',
    rate: '',
    uom_id: '',
    location: '',
    machinery_id: '',
    machinery_model: '',
    manufacturer: '',
    operational_status: '',
    date_of_purchase: '',
    warranty_expired_on: '',
    created_by: '',
  });

  useEffect(() => {
    if (Number(routeParams?.id)) {
      const fetchOne = async () => {
        const data = await machineryService.getOneMachineryByID(
          Number(routeParams?.id)
        );
        setInitialValues({
          machinery_id: data?.data?.machinery_id,
          machinery_name: data?.data?.machinery_name,
          machinery_type: data?.data?.machinery_type,
          rate: Number(data?.data?.rate),
          uom_id: data?.data?.uom_id,
          location: data?.data?.location,
          machinery_model: data?.data?.machinery_model,
          manufacturer: data?.data?.manufacturer,
          operational_status: data?.data?.operational_status,
          date_of_purchase: dateFormat(data?.data?.date_of_purchase),
          warranty_expired_on: dateFormat(data?.data?.warranty_expired_on),
          created_by: data?.data?.created_by,
        });
      };
      fetchOne();
    }
  }, [routeParams?.id]);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (Number(routeParams?.id)) {
        const Object: any = {
          machinery_id: values.machinery_id,
          machinery_name: values.machinery_name,
          machinery_type: values.machinery_type,
          rate: Number(values.rate),
          uom_id: Number(values.uom_id),
          location: values.location,
          machinery_model: values.machinery_model,
          manufacturer: values.manufacturer,
          operational_status: values.operational_status,
          date_of_purchase: values.date_of_purchase,
          warranty_expired_on: values.warranty_expired_on,
          updated_by: userData.user_id,
        };
        updateOneMachinery(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Machinery Edited');
              setOpenSnack(true);
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
            }
          },
        });
      } else {
        const Object: any = {
          machinery_name: values.machinery_name,
          machinery_type: values.machinery_type,
          rate: Number(values.rate),
          uom_id: Number(values.uom_id),
          location: values.location,
          machinery_model: values.machinery_model,
          manufacturer: values.manufacturer,
          operational_status: values.operational_status,
          date_of_purchase: values.date_of_purchase,
          warranty_expired_on: values.warranty_expired_on,
          created_by: userData.user_id,
        };
        createNewMachinery(Object, {
          onSuccess: (data, variables, contect) => {
            if (data?.message === 'success') {
              setMessage('Machinery created');
              setOpenSnack(true);
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
            }
          },
        });
      }
    },
  });

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  return (
    <div>
      {/* <div className={Styles.box}>
        <div>
          <h3>{routeParams.id ? 'Machinery Edit' : 'Machinery Add'}</h3>
        </div>
      </div>
      <div className={Styles.dividerStyle}></div> */}
      <div>
        <ProjectSubheader
          title={routeParams.id ? 'MACHINERY EDIT' : 'NEW MACHINERY'}
          navigation="/settings"
          description=""
        />
      </div>
      <div className={Styles.form}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.formFields}>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="machinery_name"
                  label="Machinery Name"
                  placeholder="Enter Machinery Name"
                  mandatory={true}
                  value={formik.values.machinery_name}
                  onChange={formik.handleChange}
                  width="350px"
                  error={
                    formik.touched.machinery_name &&
                    formik.errors.machinery_name
                  }
                />
              </div>
              <div>
                <Input
                  name="machinery_type"
                  label="Machinery Type"
                  placeholder="Enter Machinery Type"
                  mandatory={true}
                  value={formik.values.machinery_type}
                  onChange={formik.handleChange}
                  width="350px"
                  error={
                    formik.touched.machinery_type &&
                    formik.errors.machinery_type
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="rate"
                  label="Rate"
                  placeholder="Enter Rate"
                  mandatory={true}
                  value={formik.values.rate}
                  onChange={formik.handleChange}
                  width="350px"
                  error={formik.touched.rate && formik.errors.rate}
                />
              </div>
              <div>
                <Select
                  name="uom_id"
                  label="UOM"
                  defaultLabel="Select from options"
                  mandatory={true}
                  width="350px"
                  value={formik.values.uom_id}
                  onChange={formik.handleChange}
                  error={formik.touched.uom_id && formik.errors.uom_id}
                >
                  {getAllUomList?.map((option: any) => (
                    <option key={option.uom_id} value={option.uom_id}>
                      {option.name}
                    </option>
                  ))}
                </Select>
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="machinery_model"
                  label="Machinery Model"
                  placeholder="Enter Machinery Model"
                  value={formik.values.machinery_model}
                  onChange={formik.handleChange}
                  width="350px"
                  error={
                    formik.touched.machinery_model &&
                    formik.errors.machinery_model
                  }
                />
              </div>
              <div>
                <Input
                  name="manufacturer"
                  label="Manufacturer"
                  placeholder="Enter Manufacturer"
                  value={formik.values.manufacturer}
                  onChange={formik.handleChange}
                  width="350px"
                  error={
                    formik.touched.manufacturer && formik.errors.manufacturer
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="location"
                  label="Location"
                  placeholder="Enter Location"
                  value={formik.values.location}
                  onChange={formik.handleChange}
                  width="350px"
                  error={formik.touched.location && formik.errors.location}
                />
              </div>
              <div>
                <Input
                  name="operational_status"
                  label="Operational Status"
                  placeholder="Enter Operational Status"
                  value={formik.values.operational_status}
                  onChange={formik.handleChange}
                  width="350px"
                  error={
                    formik.touched.operational_status &&
                    formik.errors.operational_status
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <DatePicker
                  width="350px"
                  label="Date of Purchase"
                  name="date_of_purchase"
                  value={formik.values.date_of_purchase}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.date_of_purchase &&
                    formik.errors.date_of_purchase
                  }
                />
              </div>
              <div>
                <DatePicker
                  width="350px"
                  label="Warranty Expire Date"
                  name="warranty_expired_on"
                  value={formik.values.warranty_expired_on}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.warranty_expired_on &&
                    formik.errors.warranty_expired_on
                  }
                />
              </div>
            </div>
            <div className={Styles.buttonFields}>
              {/* <div>
                <Button
                  color="secondary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  onClick={() => {
                    navigate('/settings');
                  }}
                >
                  Back
                </Button>
              </div> */}
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  type="submit"
                >
                  Save
                </Button>
              </div>
            </div>
          </div>
        </form>
      </div>
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

export default AddMachinery;
