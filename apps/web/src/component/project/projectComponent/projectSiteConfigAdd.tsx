import React, { useEffect, useState } from 'react';
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
import {
  createprojectSite,
  updateprojectSite,
} from '../../../hooks/projectSite-hooks';
import projectSiteService from '../../../service/projectSite-service';

const ProjectSiteConfigAdd: React.FC = (props: any) => {
  console.log('props.mode', props.mode);

  const [initialValues, setInitialValues] = useState<any>({
    site_id: '',
    estimated_budget: '',
    actual_budget: '',
    approvar_id: '',
    status: 'Not Started',
    is_delete: 'N',
    address: '',
    project_id: Number(props.projectID),
    created_by: 1,
    project_site_id: '',
  });
  const { data: getAllSite = [] } = useGetAllSiteDrops();
  const { data: getAllUsersDatadrop = [] } = useGetAllUsersDrop();
  const handleClose = () => {
    props.setOpen(false);
  };
  const { mutate: postProjectSite } = createprojectSite();
  const { mutate: updateProjectSite } = updateprojectSite();
  useEffect(() => {
    const fetchData = async () => {
      const getData = await projectSiteService.getOneprojectSiteByID(
        props.projectSiteId
      );
      setInitialValues({
        site_id: getData?.data?.site_id,
        estimated_budget: getData?.data?.estimated_budget,
        actual_budget: getData?.data?.actual_budget,
        approvar_id: getData?.data?.approvar_id,
        status: getData?.data?.status,
        is_delete: 'N',
        address: '',
        project_id: Number(props.projectID),
        created_by: 1,
        project_site_id: getData?.data?.project_site_id,
      });
      console.log('getData', getData);
    };
    if (props.mode === 'EDIT') fetchData();
  }, [props.mode]);
  const validationSchema = Yup.object().shape({
    site_id: Yup.string()
      .trim()
      .required('Site is required')
      .test(
        'unique-site-ids',
        'Site name repeated are not allowed',
        function (sites: any) {
          if (props.mode === 'ADD') {
            const isSiteIdUnique = props.siteConfigData.every(
              (siteData) => siteData.site_id !== parseInt(sites, 10)
            );
            return isSiteIdUnique;
          } else {
            return true;
          }
        }
      ),
    approvar_id: Yup.string().trim().required('Approver is required'),
    actual_budget: Yup.string().matches(/^[0-9]*$/, 'Only numbers are allowed'),
    estimated_budget: Yup.string()
      .matches(/^[0-9]*$/, 'Only numbers are allowed')
      .required('Budget is required')
      .typeError('Only numbesqswqsrs are allowed')
      .test(
        'site-budget',
        'Site budget is greater than estimated budget',
        async function (budget: any) {
          const estimated_budget = props.projectData.estimated_budget;
          const site_configuration = props.siteConfigData;
          if (site_configuration.length === 0) {
            if (Number(budget) > Number(estimated_budget)) {
              return false;
            }
            return true;
          } else {
            const totalEstimation = site_configuration.reduce(
              (total: any, site: any) => total + Number(site.estimated_budget),
              0
            );
            const finalTotal = totalEstimation + Number(budget);
            if (finalTotal > estimated_budget) {
              return false;
            }
            return true;
          }
        }
      ),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      console.log('values', values);
      if (props?.mode == 'ADD') {
        postProjectSite(values, {
          onSuccess(data, variables, context) {
            console.log('data', data);
            props.setOpen(false);
            props.setMessage('Project Site created Successfully');
            props.setOpenSnack(true);
            props.setReload(true);
          },
        });
      } else {
        updateProjectSite(values, {
          onSuccess(data, variables, context) {
            console.log('data', data);
            if (data.status === true) {
              props.setOpen(false);
              props.setMessage('Project Site Updated Successfully');
              props.setOpenSnack(true);
              props.setReload(true);
            }
          },
        });
      }
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
                  disabled={props.mode === 'EDIT' ? true : false}
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
                  error={formik.errors?.approvar_id}
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
