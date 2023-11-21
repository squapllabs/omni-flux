import React, { useEffect, useState } from 'react';
import Styles from '../../styles/newStyles/projectAbstract.module.scss';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
import { useNavigate, useParams } from 'react-router-dom';
import categoryService from '../../service/category-service';
import { formatBudgetValue } from '../../helper/common-function';
import BomList from './projectBoqList';
import ClipboardIcon from '../menu/icons/clipboardIcon';
import { useGetByBOMDetails } from '../../hooks/category-hooks';

const ProjectAbstract = () => {
  const navigate = useNavigate();
  const routeparams = useParams();
  const obj: any = {
    projectId: Number(routeparams?.projectId),
    boQId: Number(routeparams?.bomconfigId),
  };
  const { data: getBomData } = useGetByBOMDetails({
    projectId: Number(routeparams?.projectId),
    boQId: Number(routeparams?.bomconfigId),
  });

  return (
    <div>
      <div className={Styles.container}>
        <div className={Styles.sub_header}>
          <div style={{ display: 'flex' }}>
            <div
              className={Styles.logo}
              onClick={() => {
                navigate(`/project-edit/${routeparams?.projectId}`);
              }}
            >
              <PreviousPageIcon width={15} height={15} color="#7f56d9" />
              <span>Back to BoQ List</span>
            </div>
            <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div style={{ display: 'flex', alignItems: 'center', gap: '20px' }}>
              <div>
                <ClipboardIcon width={30} height={30} />
              </div>
              <div className={Styles.textContent_1}>
                <span className={Styles.projectTitle}>
                  {
                    getBomData?.bom_configuration_data?.project_data
                      ?.project_name
                  }
                </span>
              </div>
            </div>
            <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div className={Styles.countContent}>
              <h3>{getBomData?.bom_configuration_data?.bom_description}</h3>
            </div>
          </div>
          <div className={Styles.boqAmount}>
          </div>
        </div>
        <div className={Styles.selected}></div>
        <div>
          <BomList bom_details={getBomData?.bom_configuration_data} />
        </div>
      </div>
    </div>
  );
};

export default ProjectAbstract;
