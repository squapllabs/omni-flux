import React, { useState, useEffect } from 'react';
import Styles from '../../styles/user.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getUsereditYupschema } from '../../helper/constants/user-constants';
import { getByuserID, updateUser } from '../../hooks/user-hooks';
import { useGetAllRoles } from '../../hooks/userRole-hooks';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Select from '../ui/Select';
import userService from '../../service/user-service';
import AddIcon from '../menu/icons/addIcon';
import { format } from 'date-fns';
import Checkbox from '../ui/Checkbox';
import CustomSnackbar from '../ui/customSnackBar';
import CustomLoader from '../ui/customLoader';
const validationSchema = getUsereditYupschema(Yup);
const UserEdit = () => {
  const navigate = useNavigate();
  const routeParams = useParams();
  const { data: getOneuserData, isLoading } = getByuserID(
    Number(routeParams?.id)
  );

  const { mutate: updateUserData } = updateUser();
  const { data: getAllRoles = [] } = useGetAllRoles();
  const [OpenSnackbar, setOpenSnakBar] = useState(false);
  const [message, setMessage] = useState('');
  const [imageUrl, setImageUrl] = useState('');
  const [checked, setChecked] = useState(false);
  const handleSnackBarClose = () => {
    setOpenSnakBar(false);
  };
  const [initialValues, setInitialValues] = useState({
    first_name: '',
    last_name: '',
    email_id: '',
    contact_no: '',
    user_status: '',
    role_id: '',
    department: '',
    gender: '',
    date_of_birth: '',
    address: {
      street: '',
      city: '',
      state: '',
      country: '',
      pin_code: '',
    },
    profile_image_url: '',
    is_two_factor: '',
  });

  useEffect(() => {
    if (getOneuserData) {
      const dateOfBirth = getOneuserData?.userProfileData?.date_of_birth;
      let formattedDate = '';
      if (dateOfBirth) {
        const currentDate = new Date(dateOfBirth);
        formattedDate = format(currentDate, 'yyyy-MM-dd');
      }
      setInitialValues({
        first_name: getOneuserData?.userData?.first_name || '',
        last_name: getOneuserData?.userData?.last_name || '',
        email_id: getOneuserData?.userData?.email_id || '',
        contact_no: getOneuserData?.userData?.contact_no || '',
        user_status: getOneuserData?.userData?.user_status || '',
        role_id: getOneuserData?.roleId || '',
        department: getOneuserData?.userData?.department || '',
        gender: getOneuserData?.userProfileData?.gender || '',
        date_of_birth: formattedDate || '',
        address: {
          street: getOneuserData?.userProfileData?.address?.street || '',
          city: getOneuserData?.userProfileData?.address?.city || '',
          state: getOneuserData?.userProfileData?.address?.state || '',
          country: getOneuserData?.userProfileData?.address?.country || '',
          pin_code: getOneuserData?.userProfileData?.address?.pin_code || '',
        },
        profile_image_url:
          getOneuserData?.userProfileData?.profile_image_url || '',
        is_two_factor: getOneuserData?.userData?.is_two_factor || '',
      });
      setChecked(getOneuserData?.userData?.is_two_factor);
    }
  }, [getOneuserData]);
  const options = [
    { value: 'AC', label: 'Active' },
    { value: 'IN', label: 'In-Active' },
  ];

  const optionsGender = [
    { value: 'male', label: 'Male' },
    { value: 'female', label: 'Female' },
  ];
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedRoleId = event.target.value;
    formik.setFieldValue('role_id', selectedRoleId);
  };
  const handleDropdownChangeStatus = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectStatus = event.target.value;
    formik.handleChange(event);
    formik.setFieldValue('user_status', selectStatus);
  };
  const handleDropdownGenderChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectGender = event.target.value;
    formik.setFieldValue('gender', selectGender);
  };

  const handleImg = async (e: any) => {
    if (e.target.files[0]) {
      try {
        const url = await userService.user_profile_upload(e.target.files[0]);
        setImageUrl(url.data);
      } catch (error) {
        console.log('Error in occur user_profile_upload:', error);
      }
    }
  };

  const handleTwoFactor = (e: any) => {
    const CheckboxValue = e.target.checked;
    setChecked(CheckboxValue);
  };

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      const Object: any = {
        first_name: values.first_name,
        last_name: values.last_name,
        email_id: values.email_id,
        user_password: getOneuserData?.user_password,
        user_status: values.user_status,
        contact_no: values.contact_no,
        role_id: values.role_id,
        user_id: Number(routeParams?.id),
        department: values.department,
        gender: values.gender,
        date_of_birth: values.date_of_birth,
        address: {
          street: values.address.street,
          city: values.address.city,
          state: values.address.state,
          country: values.address.country,
          pin_code: values.address.pin_code,
        },
        profile_image_url: imageUrl ? imageUrl : values.profile_image_url,
        is_two_factor: checked,
      };
      updateUserData(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.success === true) {
            setOpenSnakBar(true);
            setMessage('User Data Has updated Successfully');
            setTimeout(() => {
              navigate('/settings');
            }, 3000);
          }
        },
      });
    },
  });


  return (
    <div className={Styles.container}>
      <CustomLoader loading={isLoading} size={48} color="#333C44">
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.heading}>
            <h2>USER EDIT</h2>
          </div>
          <div className={Styles.fieldsOne}>
            <div className={Styles.inputField}>
              <Input
                label="First Name"
                placeholder="Enter your First name"
                name="first_name"
                value={formik.values.first_name}
                onChange={formik.handleChange}
                error={formik.touched.first_name && formik.errors.first_name}
              />
            </div>
            <div className={Styles.inputField}>
              <Input
                label="Last Name"
                placeholder="Enter your Last name"
                name="last_name"
                value={formik.values.last_name}
                onChange={formik.handleChange}
                error={formik.touched.last_name && formik.errors.last_name}
              />
            </div>
            <div className={Styles.inputField}>
              <Input
                label="Mobile Number"
                placeholder="Enter your Mobile Number"
                name="contact_no"
                value={formik.values.contact_no}
                onChange={formik.handleChange}
                error={formik.touched.contact_no && formik.errors.contact_no}
                disabled="true"
              />
            </div>
            <div className={Styles.inputField}>
              <Input
                label="Email"
                placeholder="Enter your Email"
                name="email_id"
                value={formik.values.email_id}
                onChange={formik.handleChange}
                error={formik.touched.email_id && formik.errors.email_id}
                disabled="true"
              />
            </div>
          </div>
          <div className={Styles.fieldsOne}>
            <div className={Styles.inputField}>
              <Input
                label="Department"
                placeholder="Enter Department"
                name="department"
                value={formik.values.department}
                onChange={formik.handleChange}
                error={formik.touched.department && formik.errors.department}
              />
            </div>
            <div className={Styles.inputField}>
              <span className={Styles.projectHeading}>Status</span>
              <Select
                options={options}
                onChange={handleDropdownChangeStatus}
                value={formik.values.user_status}
                defaultLabel="Select from options"
                width="100%"
              />
            </div>
            <div className={Styles.inputField}>
              <span className={Styles.projectHeading}>Role</span>
              <Select
                options={getAllRoles}
                onChange={handleDropdownChange}
                value={formik.values.role_id}
                defaultLabel="Select from options"
                width="100%"
              />
            </div>
            <div className={Styles.inputField}>
              <span className={Styles.projectHeading}>Gender</span>
              <Select
                options={optionsGender}
                onChange={handleDropdownGenderChange}
                value={formik.values.gender}
                defaultLabel="Select from options"
                width="100%"
              />
            </div>
          </div>
          <div className={Styles.fieldsOne}>
            <div className={Styles.inputField}>
              <span className={Styles.projectHeading}>Date of Birth</span>
              <div className={Styles.dateField}>
                <input
                  type="date"
                  name="date_of_birth"
                  onChange={formik.handleChange}
                  value={formik.values.date_of_birth}
                  className={Styles.datePicker}
                />
              </div>
            </div>
            <div className={Styles.inputField}>
              <Input
                label="Street"
                placeholder="Enter Street Name"
                name="address.street"
                value={formik.values.address.street}
                onChange={formik.handleChange}
                error={
                  formik.touched.address?.street &&
                  formik.errors.address?.street
                }
              />
            </div>
            <div className={Styles.inputField}>
              <Input
                label="City"
                placeholder="Enter City Name"
                name="address.city"
                value={formik.values.address.city}
                onChange={formik.handleChange}
                error={
                  formik.touched.address?.city && formik.errors.address?.city
                }
              />
            </div>
            <div className={Styles.inputField}>
              <Input
                label="State"
                placeholder="Enter State Name"
                name="address.state"
                value={formik.values.address.state}
                onChange={formik.handleChange}
                error={
                  formik.touched.address?.state && formik.errors.address?.state
                }
              />
            </div>
          </div>
          <div className={Styles.fieldsOne}>
            <div className={Styles.inputField}>
              <Input
                label="Country"
                placeholder="Enter Country Name"
                name="address.country"
                value={formik.values.address.country}
                onChange={formik.handleChange}
                error={
                  formik.touched.address?.country &&
                  formik.errors.address?.country
                }
              />
            </div>
            <div className={Styles.inputField}>
              <Input
                label="Pin Code"
                placeholder="Enter Pin Code"
                name="address.pin_code"
                value={formik.values.address.pin_code}
                onChange={formik.handleChange}
                error={
                  formik.touched.address?.pin_code &&
                  formik.errors.address?.pin_code
                }
              />
            </div>
            <div className={Styles.inputField}>
              <img
                src={
                  imageUrl ||
                  formik.values.profile_image_url ||
                  '/loginImage.png'
                }
                alt="Uploaded preview"
                className={Styles.imageset}
              />
              <label htmlFor="upload-photo" className="custom-file-upload">
                <AddIcon />
                Upload Image
              </label>
              <input
                id="upload-photo"
                name="upload_photo"
                type="file"
                accept="image/*"
                hidden
                onChange={(e) => handleImg(e)}
              />
            </div>
            <div className={Styles.inputField}>
              <div className={Styles.twoFactorCheckbox}>
                <Checkbox
                  name="is_two_factor"
                  checked={checked}
                  onChange={(e) => handleTwoFactor(e)}
                  label="Two Factor Authentication"
                />
              </div>
            </div>
          </div>
          <div className={Styles.footerButton}>
            <div className={Styles.submitButton}>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                onClick={() => navigate('/settings')}
              >
                Back
              </Button>
              <Button
                type="submit"
                color="primary"
                shape="rectangle"
                justify="center"
              >
                Submit
              </Button>
            </div>
          </div>
        </form>
        <CustomSnackbar
          open={OpenSnackbar}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </CustomLoader>
    </div>
  );
};

export default UserEdit;
