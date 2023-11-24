import React, { useState } from 'react';
// import Input from '../../ui/Input';
import Styles from '../../../styles/newStyles/reportModule/reportForm.module.scss';
// import Select from '../../ui/selectNew';
// import DatePicker from '../../ui/CustomDatePicker';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import * as yup from 'yup';
import CustomLoader from '../../ui/customLoader';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import reportService from '../../../service/report-service';
import ProjectInwardReport from '../../reportGenerator/excelReport/projectInwardReport';
const ProjectInward: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState<any>({
    project_name: '',
  });
  const [loader, setLoader] = useState(false);
  const validationSchema = yup.object().shape({
    project_name: yup.number().required(),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      setTimeout(async () => {
        const getProjectInward = await reportService.projectInwardReport(
          values?.project_name
        );
        await ProjectInwardReport(getProjectInward?.data);
        setLoader(false);
        props.setMessage('Report Generated Successfully');
        props.setOpenSnack(true);
        props.setOpen(false);
      }, 1000);
    },
  });
  return (
    <div>
      <CustomLoader loading={loader}>
        <div className={Styles?.container}>
          <div style={{ paddingBottom: '5px' }}>
            <AutoCompleteSelect
              name="project_name"
              label="Project Name"
              defaultLabel="Select Option"
              placeholder="Select Option"
              onChange={formik.handleChange}
              value={formik.values.project_name}
              optionList={props.getAllProjectForDrop}
              onSelect={(value) => {
                formik.setFieldValue('project_name', value);
              }}
              error={
                formik.errors.project_name && formik.touched.project_name
                  ? true
                  : false
              }
              mandatory
            />
          </div>
        </div>
        <div className={Styles.dividerLine}></div>
        <div className={Styles.finalButton}>
          <div>
            <Button
              type="button"
              color="cancel"
              shape="rectangle"
              size="small"
              justify="center"
              onClick={() => {
                props.setOpen(false);
              }}
            >
              Cancel
            </Button>
          </div>
          <div>
            <Button
              type="submit"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
              onClick={formik.handleSubmit}
            >
              Generate Report
            </Button>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};
export default ProjectInward;
