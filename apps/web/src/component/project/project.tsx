import React, { useEffect, useState } from 'react';
import CustomGroupButton from '../ui/CustomGroupButton';
import Button from '../ui/Button';
import Styles from '../../styles/project.module.scss';
import ProjectGeneralDetails from './projectComponent/projectGeneralDetails';
import ProjectBomConfig from './projectComponent/projectBomConfig';
import ProjectSiteConfig from './projectComponent/projectSiteConfig';
import CustomLoader from '../ui/customLoader';
import { useParams, useNavigate } from 'react-router-dom';
import { getByProjectId } from '../../hooks/project-hooks';
import projectService from '../../service/project-service';
import ProjectSettings from './projectComponent/projectSettings';
const Project = () => {
  const routeParams = useParams();

  const [buttonLabels, setButtonLabels] = useState([
    { label: 'General settings', value: 'PGS' },
    { label: 'Site Configuration', value: 'PSC' },
    { label: 'BOM configuration', value: 'PBC' },
    { label: 'Settings', value: 'PSG' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('PGS');
  const [loader, setLoader] = useState(false);
  const [reload, setReload] = useState(false);
  const [projectData, setProjectData] = useState<any>({});
  useEffect(() => {
    const fetchData = async () => {
      const getData = await projectService.getOneProjectById(
        Number(routeParams?.id)
      );
      setProjectData(getData);
    };
    if (routeParams?.id != undefined) fetchData();
  }, [loader]);
  const handleGroupButtonClick = (value: string) => {
    if (routeParams?.id != undefined) {
      setActiveButton(value);
    } else {
      setActiveButton('PGS');
    }
  };
  return (
    <CustomLoader loading={loader} size={20}>
      <div className={Styles.Container}>
        <div className={Styles.Container_main}>
          <div className={Styles.box}>
            {routeParams?.id != undefined ? (
              <div className={Styles.mainTextContent}>
                <div className={Styles.textContent_1}>
                  <h3>{projectData?.data?.project_name}</h3>
                  <span className={Styles.content}>
                    {projectData?.data?.description}
                  </span>
                </div>
                <div className={Styles.groupButton}>
                  <div>
                    <CustomGroupButton
                      labels={buttonLabels}
                      onClick={handleGroupButtonClick}
                      activeButton={activeButton}
                    />
                  </div>
                </div>
              </div>
            ) : (
              <div className={Styles.textContent}>
                <h3>Add - Project</h3>
                <span className={Styles.content}>Add your project</span>
              </div>
            )}
          </div>
          <div className={Styles.box}>
            <div className={Styles.mainBody}>
              {activeButton === 'PGS' ? (
                <ProjectGeneralDetails
                  setActiveButton={setActiveButton}
                  setLoader={setLoader}
                  loader={loader}
                />
              ) : (
                ''
              )}
              {activeButton === 'PBC' ? (
                <ProjectBomConfig
                  setActiveButton={setActiveButton}
                  setLoader={setLoader}
                  loader={loader}
                />
              ) : (
                ''
              )}
              {activeButton === 'PSC' ? (
                <ProjectSiteConfig
                  setActiveButton={setActiveButton}
                  setLoader={setLoader}
                  loader={loader}
                />
              ) : (
                ''
              )}
              {activeButton === 'PSG' ? (
                <ProjectSettings
                  setActiveButton={setActiveButton}
                  setLoader={setLoader}
                  loader={loader}
                  projectData = {projectData}
                />
              ) : (
                ''
              )}
            </div>
          </div>
        </div>
      </div>
    </CustomLoader>
  );
};

export default Project;
