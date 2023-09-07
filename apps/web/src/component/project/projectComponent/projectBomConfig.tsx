import React, { useState } from 'react';
import Styles from '../../../styles/projectForm.module.scss';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import Input from '../../ui/Input';
import Select from '../../ui/selectNew';

const ProjectBomConfig: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState();
  return (
    <div>
      <div className={Styles.tableContainer}>
        <div>
          <table>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>S No</th>
                <th className={Styles.tableHeadingSite}>BOM Name</th>
                <th className={Styles.tableHeading}>BOM Description</th>
                <th className={Styles.tableHeading}>BOM Type</th>
                <th className={Styles.tableHeading}>Action</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>1</td>
                <td>
                  <Input />
                </td>
                <td>
                  <Input />
                </td>
                <td>
                  <Select></Select>
                </td>
              </tr>
            </tbody>
          </table>
          <div className={Styles.buttonContent}>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
              icon={<AddIcon />}
              onClick={(e) => props.setActiveButton('PGS')}
            >
              BOM CONFIG
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ProjectBomConfig;
