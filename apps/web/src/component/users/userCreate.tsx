import React, { useState } from 'react';
import Styles from '../../styles/user.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getUsercreationYupschema } from '../../helper/constants/user-constants';
import { useCreateUser } from '../../hooks/user-hooks';
import { useGetAllRole } from '../../hooks/userRole-hooks';
import { useNavigate } from 'react-router';
import Input from '../ui/Input';
import Button from '../ui/Button';
import { BsFillEyeSlashFill, BsFillEyeFill } from 'react-icons/bs';
import { FaLock } from 'react-icons/fa6';
import Select from '../ui/selectNew';
import userService from '../../service/user-service';
import AddIcon from '../menu/icons/addIcon';
import CustomSnackbar from '../ui/customSnackBar';
import ProjectSubheader from '../project/projectSubheader';
import DatePicker from '../ui/CustomDatePicker';
import CloseIcon from '../menu/icons/closeIcon';

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
  // const [selectedFile, setSelectedFile] = useState<File | null>(null);

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

  const { mutate: createNewusers } = useCreateUser();
  const { data: getAllRoles = [] } = useGetAllRole();

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

  const handleRemoveFile = () => {
    setUserImage('');
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
        role_id: Number(values.role_id),
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
          if (data?.message === 'success') {
            setMessage('User created successfully');
            setOpenSnakBar(true);
            setTimeout(() => {
              navigate('/settings');
            }, 3000);
          } else {
            setIsWarning(true);
            setMessage('Error occured in creating a User !');
            setOpenSnakBar(true);
          }
        },
      });
    },
  });
  return (
    <div>
      <div>
        <ProjectSubheader
          title="NEW USER"
          navigation="/settings"
          description=""
        />
      </div>
      <div className={Styles.container}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.box}>
            <div className={Styles.fieldsOne}>
              <div className={Styles.inputField}>
                <Input
                  label="First Name"
                  placeholder="Enter first name"
                  name="first_name"
                  mandatory={true}
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
                  mandatory={true}
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
                  mandatory={true}
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
                  mandatory={true}
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
                      className={Styles.passToggle}
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
                  mandatory={true}
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
                  mandatory={true}
                  value={formik.values.department}
                  onChange={formik.handleChange}
                  error={formik.touched.department && formik.errors.department}
                />
              </div>
              <div className={Styles.inputField}>
                <Select
                  name="role_id"
                  label="Role"
                  defaultLabel="Select from Options"
                  mandatory={true}
                  value={formik.values.role_id}
                  onChange={formik.handleChange}
                  error={formik.touched.role_id && formik.errors.role_id}
                >
                  {getAllRoles?.map((option: any) => (
                    <option key={option.role_id} value={option.role_id}>
                      {option.role_name}
                    </option>
                  ))}
                </Select>
              </div>
              <div className={Styles.inputField}>
                <Select
                  name="gender"
                  label="Gender"
                  defaultLabel="Select from Options"
                  value={formik.values.gender}
                  onChange={formik.handleChange}
                  error={formik.touched.gender && formik.errors.gender}
                >
                  {options?.map((option: any) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </Select>
              </div>
            </div>
            <div className={Styles.fieldsOne}>
              <div className={Styles.dateArea}>
                <span className={Styles.projectHeading}>Date of Birth</span>
                <div className={Styles.dateField}>
                  <DatePicker
                    name="date_of_birth"
                    onChange={formik.handleChange}
                    value={formik.values.date_of_birth}
                    className={Styles.datePicker}
                    error={
                      formik.touched.date_of_birth &&
                      formik.errors.date_of_birth
                    }
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
                    formik.touched.address?.state &&
                    formik.errors.address?.state
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
                      className={Styles.imageset}
                    />
                  )}
                  {userImage ? (
                    <span className={Styles.cancel}>
                      <button className={Styles.closeButton}>
                        <CloseIcon onClick={handleRemoveFile} />
                      </button>
                    </span>
                  ) : (
                    ''
                  )}
                  <label htmlFor="upload-photo">
                    {!userImage ? <AddIcon /> : ''}
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
                  type="submit"
                  color="primary"
                  shape="rectangle"
                  justify="center"
                >
                  Submit
                </Button>
              </div>
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
    </div>
  );
};

export default UserCreate;
