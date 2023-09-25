import React from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import Styles from '../../styles/projectInfo.module.scss';
import CustomCard from '../ui/CustomCard';
import { getByProjectId } from '../../hooks/project-hooks';
import { format } from 'date-fns';
import BackArrow from '../menu/icons/backArrow';

const ProjectView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const ProjectId = Number(routeParams?.id);
  const { data: getOneProject } = getByProjectId(ProjectId);

  return (
    <div>
      <div className={Styles.title}>
        <h2>Project Information</h2>
        {/* <Button
          text="Back"
          backgroundColor="#7F56D9"
          fontSize={14}
          fontWeight={500}
          width={100}
          icon={<BackArrow />}
          onClick={() => navigate('/project-list')}
        /> */}
        <Button
                type="button"
                color="primary"
                shape="rectangle"
                size="small"
                justify="center"
                icon={<BackArrow />}
                onClick={() => navigate('/project-list')}
              >
                Back
              </Button>
      </div>
      <div className={Styles.cardContent}>
        <CustomCard>
          <div className={Styles.mainContent}>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Project Name</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.project_name
                  ? `${getOneProject?.project_name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Client Name</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.client.name
                  ? `${getOneProject?.client.name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Description</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.description
                  ? `${getOneProject?.description}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Status</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.status
                  ? `${getOneProject?.status}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Manager</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.user?.first_name
                  ? `${getOneProject?.user?.first_name} ${
                      getOneProject?.user?.last_name
                        ? getOneProject.user?.last_name
                        : ''
                    }`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Start Date</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.date_started
                  ? `${format(
                      new Date(getOneProject?.date_started),
                      'MMM dd, yyyy'
                    )}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>End Date</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.date_ended
                  ? `${format(
                      new Date(getOneProject?.date_ended),
                      'MMM dd, yyyy'
                    )}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Estimated Budget</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.estimated_budget
                  ? `${getOneProject?.estimated_budget}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Actual Budget</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.actual_budget
                  ? `${getOneProject?.actual_budget}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Project Sites</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.project_site.map((site: any) => (
                  <ul className={Styles.siteList}>
                    <li key={site.project_site_id}>
                      {site.site_details?.name
                        ? `${site.site_details.name}   (${site.status})`
                        : 'Not Provided'}
                    </li>
                  </ul>
                ))}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Project Notes</div>
              <div className={Styles.rightData}>
                {' '}
                {getOneProject?.project_notes
                  ? `${getOneProject?.project_notes}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Uploaded Documents</div>
              <div className={Styles.rightData}>
                <ul className={Styles.siteList}>
                  {getOneProject?.project_documents.length > 0 ? (
                    getOneProject.project_documents.map(
                      (document: any, index: any) => (
                        <li key={index}>
                          <a
                            href={document.path}
                            target="_blank"
                            rel="noopener noreferrer"
                          >
                            Document {index + 1}
                          </a>
                        </li>
                      )
                    )
                  ) : (
                    <li>-</li>
                  )}
                </ul>
              </div>
            </div>
          </div>
        </CustomCard>
      </div>
    </div>
  );
};

export default ProjectView;
