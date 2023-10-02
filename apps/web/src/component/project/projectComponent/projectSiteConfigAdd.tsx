import React, { useState } from 'react';
import { useGetAllSiteDrops } from '../../../hooks/site-hooks';
import {
  useGetAllUsersDrop,
  useGetAllUsers,
  getUserbyRole,
} from '../../../hooks/user-hooks';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import Styles from '../../../styles/newStyles/project_siteConfig.module.scss';
import Input from '../../ui/Input';
import Button from '../../ui/Button';

const ProjectSiteConfigAdd: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState<any>({
    site_id: '',
    estimated_budget: '',
    actual_budget: '',
    approvar_id: '',
    status: 'Not Started',
    is_delete: 'N',
    address: '',
  });
  const { data: getAllSite = [] } = useGetAllSiteDrops();
  const { data: getAllUsersDatadrop = [] } = useGetAllUsersDrop();
  const handleClose = () => {
    props.setOpen(false);
  };
  const formik = useFormik({
    initialValues,
    // validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      console.log('values', values);
    },
  });
  return (
    <div className={Styles.container}>
      <form></form>
      <div className={Styles.sub_container}>
        <div className={Styles.sub_sub_container}>
          <div className={Styles.divOne}>
            <div style={{ width: '60%' }}>
              <div className={Styles.field}>
                <AutoCompleteSelect
                  width="185%"
                  name="site_id"
                  defaultLabel="Select Site"
                  placeholder="Select from options"
                  label="Site"
                  value={formik.values.site_id}
                  onSelect={(datas) => {
                    formik.setFieldValue('site_id', datas);
                  }}
                  error={formik.errors?.site_id}
                  optionList={getAllSite}
                />
              </div>
              <div className={Styles.field}>
                <Input
                  name="estimated_budget"
                  label="Estimated Budget"
                  placeholder="Enter your Estimated Budget"
                  value={formik.values.estimated_budget}
                  onChange={formik.handleChange}
                  mandatory
                  error={
                    formik.touched.estimated_budget &&
                    formik.errors.estimated_budget
                  }
                  width="185%"
                />
              </div>
              <div className={Styles.field}>
                <Input
                  name="actual_budget"
                  label="Actual Budget"
                  placeholder="Enter your Actual Budget"
                  value={formik.values.actual_budget}
                  onChange={formik.handleChange}
                  mandatory
                  error={
                    formik.touched.actual_budget && formik.errors.actual_budget
                  }
                  width="185%"
                />
              </div>
              <div className={Styles.field}>
                <AutoCompleteSelect
                  width="185%"
                  name="approvar_id"
                  label="Approver"
                  defaultLabel="Select Site"
                  placeholder="Select from options"
                  value={formik.values.approvar_id}
                  onSelect={(datas) => {
                    formik.setFieldValue('approvar_id', datas);
                  }}
                  error={formik.errors?.getAllUsersDatadrop}
                  optionList={getAllUsersDatadrop}
                />
              </div>
            </div>
            <div
              style={{
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
              }}
            >
              <img src="/siteAdd.png" alt="aa" width="75%" height="75%" />
            </div>
          </div>
        </div>
        <div className={Styles.sub_sub_container_2}>
          <div className={Styles.footer}>
            <div>
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
                  onClick={formik.handleSubmit}
                >
                  Save
                </Button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ProjectSiteConfigAdd;
