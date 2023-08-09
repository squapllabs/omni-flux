import React from 'react';
import Styles from '../../styles/projectWorkBreakDownForm.module.scss';
// import { useFormik } from 'formik';
// import * as Yup from 'yup';
// import Input from '../ui/Input';

const ProjectWorkBreakForm = () => {
  return (
    <div className={Styles.container}>
      <div className={Styles.textContent}>
        <h3>Add - Work Break Down</h3>
        <span className={Styles.content}>Add your work break down</span>
      </div>
    </div>
  );
};

export default ProjectWorkBreakForm;
