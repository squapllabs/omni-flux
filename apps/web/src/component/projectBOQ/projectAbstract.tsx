import React, { useEffect, useState } from 'react';
import Styles from '../../styles/newStyles/projectAbstract.module.scss';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
import { useNavigate, useParams } from 'react-router-dom';
import categoryService from '../../service/category-service';
import { formatBudgetValue } from '../../helper/common-function';
import BomList from './projectBoqList';
import ClipboardIcon from '../menu/icons/clipboardIcon';
import { useGetByBOMDetails } from '../../hooks/category-hooks';
const projectAbstract = () => {
  const useNavigation = useNavigate();
  const useRouteParams = useParams();
  const [overallAbstractValue, setOverallAbstractValue] = useState<any>(0);
  // const [reload, setReload] = useState(false);
  const obj: any = {
    projectId: Number(useRouteParams?.projectId),
    boQId: Number(useRouteParams?.bomconfigId),
  };
  const { data: getBomData } = useGetByBOMDetails(obj);

  // const getOverallAbsctractValue = (data) => {
  //   setOverallAbstractValue(data);
  // };

  return (
    <div>
      <div className={Styles.container}>
        <div className={Styles.sub_header}>
          <div style={{ display: 'flex' }}>
            <div
              className={Styles.logo}
              onClick={() => {
                useNavigation(`/project-edit/${useRouteParams?.projectId}`);
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
                {/* <span className={Styles.content}>
                  {getBomData?.bom_configuration_data?.bom_name}
                </span> */}
              </div>
            </div>
            <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div className={Styles.countContent}>
              {/* <h3>{getBomData?.abstract_count}</h3> */}
              <h3>{getBomData?.bom_configuration_data?.bom_description}</h3>
            </div>
            {/* <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div className={Styles.countContent}>
              <h3>{getBomData?.tasks_count}</h3>
              <span className={Styles.countContentTitle}>Task</span>
            </div> */}
          </div>

          <div className={Styles.boqAmount}>
            {/* <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div> */}
            {/* <div className={Styles.countContent}>
              <h3>
                {formatBudgetValue(
                  getBomData?.bom_configuration_data?.budget
                    ? getBomData?.bom_configuration_data?.budget
                    : 0
                )}
              </h3>
              <span className={Styles.countContentTitle}>Aggregated Value</span>
            </div> */}
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

export default projectAbstract;
