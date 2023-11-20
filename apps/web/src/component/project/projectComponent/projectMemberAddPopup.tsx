import React, { useState } from 'react';
import Styles from '../../../styles/projectSettings.module.scss';
import Input from '../../ui/Input';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import CustomSnackBar from '../../ui/customSnackBar';
import { getProjectMemberCreationYupschema } from '../../../helper/constants/projectSettings-constants';
import { useGetAllSelectedRoles } from '../../../hooks/userRole-hooks';
import {
  useCreateProjectMember,
  useGetBySearchProjectMembers,
} from '../../../hooks/projectSettings-hook';
import ProjectSettingsService from '../../../service/projectSettings-service';
import * as Yup from 'yup';
import DatePicker from '../../ui/CustomDatePicker';
import CustomPopup from '../../ui/CustomRightSidePopup';
import CancelIcon from '../../menu/icons/closeIcon';

const ProjectMemberAddPopup = (props: any) => {
  const [initialValues, setInitialValues] = useState({
    project_role_id: '',
    project_role_name: '',
    user_id: '',
    access_start_date: '',
    access_end_date: '',
    project_id: props.projectId,
  });
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [userData, setUserData] = useState();
  const { data: getAllRolesData = [], isLoading: dropLoading } =
    useGetAllSelectedRoles();
  const { mutate: createNewProjectMember } = useCreateProjectMember();

  const validationSchema = getProjectMemberCreationYupschema(Yup);
  const fetchData = async (data: any) => {
    const roleObj = {
      id: props.projectId,
      role: data,
    };
    const getData = await ProjectSettingsService.fetchRoleBasedUser(roleObj);
    const arr: any = [];
    const userList = getData?.data?.map((user: any, index: any) => {
      const obj: any = {
        value: user?.user_id,
        label: user?.first_name + ' ' + user?.last_name,
      };
      arr.push(obj);
    });
    setUserData(arr);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const Object: any = {
        project_id: props.projectId,
        project_role_id: values.project_role_id,
        user_id: values.user_id,
        access_start_date: values.access_start_date,
        access_end_date: values.access_end_date,
      };
      createNewProjectMember(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Project Member created');
            setOpenSnack(true);
            setTimeout(() => {
              handleClose();
            }, 1000);
            resetForm();
          }
        },
      });
    },
  });

  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.divOne}>
          <div>
            <div>
              <AutoCompleteSelect
                label="Permission Role"
                name="project_role_id"
                onChange={formik.handleChange}
                value={formik.values.project_role_id}
                placeholder="Select from options"
                width="350px"
                mandatory
                onSelect={(value) => {
                  formik.setFieldValue('project_role_id', value);
                  const matchingObjects = getAllRolesData.filter(
                    (obj: any) => Number(obj.value) === Number(value)
                  );
                  formik.setFieldValue(
                    'project_role_name',
                    matchingObjects[0]?.label
                  );
                  fetchData(matchingObjects[0]?.label);
                }}
                optionList={dropLoading === true ? [] : getAllRolesData}
                error={
                  formik.touched.project_role_id &&
                  formik.errors.project_role_id
                }
              />
            </div>
            <div>
              <AutoCompleteSelect
                label="Members"
                name="user_id"
                onChange={formik.handleChange}
                value={formik.values.user_id}
                placeholder="Select from options"
                mandatory
                width="350px"
                onSelect={(value) => {
                  formik.setFieldValue('user_id', value);
                }}
                optionList={userData}
                error={formik.touched.user_id && formik.errors.user_id}
              />
            </div>
            <div>
              <DatePicker
                label="Access Start Date"
                name="access_start_date"
                onChange={formik.handleChange}
                width="160px"
                value={formik.values.access_start_date}
                error={
                  formik.touched.access_start_date &&
                  formik.errors.access_start_date
                }
              />
            </div>
            <div>
              <DatePicker
                label="Access Expiration Date"
                name="access_end_date"
                onChange={formik.handleChange}
                width="160px"
                value={formik.values.access_end_date}
                // mandatory
                error={
                  formik.touched.access_end_date &&
                  formik.errors.access_end_date
                }
              />
            </div>
          </div>
          <div>
            <img src="/add-member.png" alt="member" className={Styles.image}></img>
          </div>
        </div>

        <div className={Styles.footer}>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.button}>
            <Button
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleClose}
              className={Styles.cancelButton}
            >
              Cancel
            </Button>
            <Button
              shape="rectangle"
              color="primary"
              justify="center"
              size="small"
              type="submit"
            >
              Save
            </Button>
          </div>
        </div>
      </form>
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

export default ProjectMemberAddPopup;
