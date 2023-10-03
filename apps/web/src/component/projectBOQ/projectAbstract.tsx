import React, { useEffect, useState } from 'react';
import Styles from '../../styles/newStyles/projectAbstract.module.scss';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
import { useNavigate, useParams } from 'react-router-dom';
import categoryService from '../../service/category-service';
import { formatBudgetValue } from '../../helper/common-function';
import BomList from './projectBoqList';
import ClipboardIcon from '../menu/icons/clipboardIcon';
const projectAbstract = () => {
  const navigate = useNavigate();
  const routeParams = useParams();
  const [bomData, setBomData] = useState<any>({});
  const [reload, setReload] = useState(false);
  useEffect(() => {
    const fatchData = async () => {
      const obj: any = {
        projectId: Number(routeParams?.projectId),
        boQId: Number(routeParams?.bomconfigId),
      };
      const getBomData = await categoryService.getBOMDetail(obj);
      setBomData(getBomData?.data);
      console.log('getBomData', getBomData);
    };
    fatchData();
  }, [reload]);
  return (
    <div>
      <div className={Styles.container}>
        <div className={Styles.sub_header}>
          <div style={{ display: 'flex' }}>
            <div
              className={Styles.logo}
              onClick={() => {
                navigate(`/project-edit/${routeParams?.projectId}`);
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
                  {bomData?.bom_configuration_data?.project_data?.project_name}
                </span>
                <span className={Styles.content}>
                  {bomData?.bom_configuration_data?.bom_name}
                </span>
              </div>
            </div>
            <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div className={Styles.countContent}>
              <h3>{bomData?.abstract_count}</h3>
              <span className={Styles.countContentTitle}>Abstract</span>
            </div>
            <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div className={Styles.countContent}>
              <h3>{bomData?.tasks_count}</h3>
              <span className={Styles.countContentTitle}>Task</span>
            </div>
          </div>

          <div className={Styles.boqAmount}>
            <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div className={Styles.countContent}>
              <h3>
                {formatBudgetValue(
                  bomData?.bom_configuration_data?.budget
                    ? bomData?.bom_configuration_data?.budget
                    : 0
                )}
              </h3>
              <span className={Styles.countContentTitle}>Aggregated Value</span>
            </div>
          </div>
        </div>
        <div className={Styles.selected}></div>
        <div>
          <BomList setReload={setReload} reload={reload} />
        </div>
      </div>
    </div>
  );
};

export default projectAbstract;
