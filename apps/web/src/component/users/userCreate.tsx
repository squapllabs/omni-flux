import React, { useState } from 'react';
import Styles from '../../styles/user.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getUsercreationYupschema } from '../../helper/constants/user-constants';
import { createUser } from '../../hooks/user-hooks';
import { useGetAllRoles } from '../../hooks/userRole-hooks';
import { useNavigate } from 'react-router';
import Input from '../ui/Input';
import Button from '../ui/Button';
import { BsFillEyeSlashFill, BsFillEyeFill } from 'react-icons/bs';
import { FaLock } from 'react-icons/fa6';
import Select from '../ui/Select';
import userService from '../../service/user-service';
import AddIcon from '../menu/icons/addIcon';
import CustomSnackbar from '../ui/customSnackBar';

const validationSchema = getUsercreationYupschema(Yup);
const UserCreate = () => {
  const navigate = useNavigate();
  const [OpenSnackbar, setOpenSnakBar] = useState(false);
  const [isWarning, setIsWarning] = useState(false);
  const [message, setMessage] = useState('');
  const [passwordShown, setPasswordShown] = useState(false);
  const [selectedValue, setSelectedValue] = useState('');
  const [selectedGenderValue, setSelectedGenderValue] = useState('');
  const [imageUrl, setImageUrl] = useState('');
  const [userImage, setUserImage] = useState();

  const handleSnackBarClose = () => {
    setOpenSnakBar(false);
  };
  const [initialValues, setInitialValues] = useState({
    first_name: '',
    last_name: '',
    username: '',
    user_password: '',
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
  });

  const togglePasswordVisibility = () => {
    setPasswordShown(!passwordShown);
  };
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
  };

  const handleDropdownGenderChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedGender = event.target.value;
    setSelectedGenderValue(selectedGender);
  };

  const { mutate: createNewusers } = createUser();
  const { data: getAllRoles = [] } = useGetAllRoles();

  const options = [
    { value: 'male', label: 'Male' },
    { value: 'female', label: 'Female' },
  ];

  const handleImg = async (e: any) => {
    if (e.target.files[0]) {
      try {
        const url = await userService.user_profile_upload(e.target.files[0]);
        setUserImage(e.target.files[0]);
        setImageUrl(url.data);
      } catch (error) {
        console.log('Error in occur user_profile_upload:', error);
      }
    }
  };

  const formik = useFormik({
    initialValues,
    validationSchema,
    onSubmit: (values) => {
      const Object: any = {
        first_name: values.first_name,
        last_name: values.last_name,
        user_password: values.user_password,
        email_id: values.email_id,
        user_status: 'AC',
        contact_no: values.contact_no,
        role_id: Number(selectedValue),
        department: values.department,
        gender: selectedGenderValue,
        date_of_birth: values.date_of_birth,
        address: {
          street: values.address.street,
          city: values.address.city,
          state: values.address.state,
          country: values.address.country,
          pin_code: values.address.pin_code,
        },
        profile_image_url: imageUrl,
      };
      createNewusers(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.success) {
            setMessage('User created successfully');
            setOpenSnakBar(true);
            setInterval(() => {
              navigate('/settings');
            }, 3000);
          } else {
            setIsWarning(true);
            setMessage(data?.message);
            setOpenSnakBar(true);
          }
        },
      });
    },
  });
  return (
    <div className={Styles.container}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.heading}>
          <h2>USER CREATION</h2>
        </div>
        <div className={Styles.fieldsOne}>
          <div className={Styles.inputField}>
            <Input
              label="First Name"
              placeholder="Enter first name"
              name="first_name"
              value={formik.values.first_name}
              onChange={formik.handleChange}
              error={formik.touched.first_name && formik.errors.first_name}
            />
          </div>
          <div className={Styles.inputField}>
            <Input
              label="Last Name"
              placeholder="Enter last name"
              name="last_name"
              value={formik.values.last_name}
              onChange={formik.handleChange}
              error={formik.touched.last_name && formik.errors.last_name}
            />
          </div>

          <div className={Styles.inputField}>
            <Input
              label="Email"
              placeholder="Enter email"
              name="email_id"
              value={formik.values.email_id}
              onChange={formik.handleChange}
              error={formik.touched.email_id && formik.errors.email_id}
            />
          </div>
          <div className={Styles.inputField}>
            <Input
              label="Password"
              placeholder="Enter password"
              name="user_password"
              type={passwordShown ? 'text' : 'password'}
              value={formik.values.user_password}
              onChange={formik.handleChange}
              error={
                formik.touched.user_password && formik.errors.user_password
              }
              prefixIcon={<FaLock />}
              suffixIcon={
                <button
                  type="button"
                  onClick={togglePasswordVisibility}
                  style={{ background: 'none', border: 'none' }}
                >
                  {passwordShown ? (
                    <BsFillEyeFill size={20} />
                  ) : (
                    <BsFillEyeSlashFill size={20} />
                  )}
                </button>
              }
            />
          </div>
        </div>
        <div className={Styles.fieldsOne}>
          <div className={Styles.inputField}>
            <Input
              label="Mobile Number"
              placeholder="Enter mobile number"
              name="contact_no"
              value={formik.values.contact_no}
              onChange={formik.handleChange}
              error={formik.touched.contact_no && formik.errors.contact_no}
            />
          </div>
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
            <span className={Styles.projectHeading}>Role</span>
            <Select
              options={getAllRoles}
              onChange={handleDropdownChange}
              value={selectedValue}
              defaultLabel="Select from options"
              width="100%"
            />
            {formik.touched.role_id && formik.errors.role_id && (
              <div className={Styles.error}>{formik.errors.role_id}</div>
            )}
          </div>
          <div className={Styles.inputField}>
            <span className={Styles.projectHeading}>Gender</span>
            <Select
              options={options}
              onChange={handleDropdownGenderChange}
              value={selectedGenderValue}
              defaultLabel="Select from options"
              width="100%"
            />
          </div>
        </div>
        <div className={Styles.fieldsOne}>
          <div style={{ width: '20%' }}>
            <span className={Styles.projectHeading}>Date of Birth</span>
            <div className={Styles.dateField}>
              <input
                type="date"
                placeholder="Date of Birth"
                name="date_of_birth"
                onChange={formik.handleChange}
                value={formik.values.date_of_birth}
                style={{
                  border: '1px solid #ccc',
                  borderRadius: '5px',
                  padding: '5px',
                  width: '95%',
                  height: '90%',
                }}
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
                formik.touched.address?.street && formik.errors.address?.street
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
            <div className={Styles.userImage}>
              {userImage && (
                <img
                  src={URL.createObjectURL(userImage)}
                  alt="Uploaded preview"
                  style={{
                    width: '50px',
                    height: '50px',
                    objectFit: 'cover',
                  }}
                />
              )}
              <label>
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
          </div>
          <div className={Styles.inputField}></div>
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
        type={isWarning ? 'error' : 'success'}
      />
    </div>
  );
};

export default UserCreate;
