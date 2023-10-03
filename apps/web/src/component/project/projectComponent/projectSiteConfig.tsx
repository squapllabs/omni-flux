import React, { ChangeEvent, useEffect, useState } from 'react';
import Button from '../../ui/Button';
import { useNavigate, useParams } from 'react-router-dom';
import CustomSnackBar from '../../ui/customSnackBar';
import projectService from '../../../service/project-service';
import Styles from '../../../styles/project.module.scss';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import { useFormik } from 'formik';
import * as yup from 'yup';
import DeleteIcon from '../../menu/icons/newDeleteIcon';
import siteService from '../../../service/site-service';
import CustomSiteAdd from '../../ui/CustomSiteAdd';
import { useGetAllSiteDrops } from '../../../hooks/site-hooks';
import {
  useGetAllUsersDrop,
  useGetAllUsers,
  getUserbyRole,
} from '../../../hooks/user-hooks';
import AddIcon from '../../menu/icons/addIcon';
import Input from '../../ui/Input';
import {
  createProject,
  useGetMasterProjectParentType,
  updateProject,
  getUserDataProjectRolebased,
} from '../../../hooks/project-hooks';
import SiteNavigateIcon from '../../menu/icons/siteNavigateIcon';
import NewEditIcon from '../../menu/icons/newEditIcon';
import CustomPopup from '../../ui/CustomRightSidePopup';
import CustomSidePopup from '../../ui/CustomSidePopup';
import ProjectSiteConfigAdd from './projectSiteConfigAdd';

import {
  createprojectSite,
  useGetAllPaginatedprojectSite,
} from '../../../hooks/projectSite-hooks';
import CustomPagination from '../../menu/CustomPagination';
import CustomLoader from '../../ui/customLoader';

const ProjectSiteConfig: React.FC = (props: any) => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const [projectData, setProjectData] = useState<any>({});
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [open, setOpen] = useState(false);
  const [projectSiteOpen, setProjectSiteOpen] = useState(false);
  const [siteConfigData, setSiteConfigData] = useState<any[]>([]);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [projectSiteId, setProjectSiteId] = useState();
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const masterData = {
    limit: 10,
    offset: 0,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    project_id: Number(routeParams?.id),
    global_search: '',
  };
  const {
    data: initialData,
    refetch,
    isLoading: getAllLoadingProjectMasterData,
  } = useGetAllPaginatedprojectSite(masterData);
  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, reload]);
  console.log('getAllLoadingProjectMasterData', initialData);
  const handleCloseSiteAdd = () => {
    setOpen(false);
  };
  const handleCloseProjectSite = () => {
    setProjectSiteOpen(false);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const handleProjectSiteEdit = (value: any) => {
    setMode('EDIT');
    setProjectSiteId(value);
    setProjectSiteOpen(true);
  };
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };
  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  return (
    <div>
      <CustomLoader
        loading={getAllLoadingProjectMasterData}
        size={48}
        color="#333C44"
      >
        {initialData?.total_count !== 0 ? (
          <div>
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                <div className={Styles.subHeading}>
                  <SiteNavigateIcon width={30} height={30} />
                  <h4>SITES</h4>
                </div>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon color="white" />}
                    onClick={() => {
                      setMode('ADD');
                      setProjectSiteOpen(true);
                    }}
                  >
                    Add Site to Project
                  </Button>
                </div>
                <div
                  className={Styles.siteCreatelabel}
                  onClick={() => {
                    setOpen(true);
                  }}
                >
                  <SiteNavigateIcon width={15} height={15} color="#7f56d9" />
                  <span className={Styles.sitelabel}>Create New Site</span>
                </div>
              </div>
            </div>

            <div className={Styles.tableContainer}>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>#</th>
                    <th>Site</th>
                    <th>Site Address</th>
                    <th>Status</th>
                    <th>Estimated Budget</th>
                    <th>Actual Budget</th>
                    <th>Approver</th>
                    <th>Action</th>
                  </tr>
                </thead>
                <tbody>
                  {initialData?.content.map((row, index) => {
                    console.log('row', row);
                    rowIndex = rowIndex + 1;
                    return (
                      <tr key={index}>
                        <td>{rowIndex}</td>
                        <td>{row.site_details?.name}</td>
                        <td>
                          {row?.siteData?.site_contractor_id === undefined ? (
                            <div>
                              <span>
                                {row.site_details?.address?.street}{' '}
                                {row.site_details?.address?.city},{' '}
                                {row.site_details?.address?.state},
                              </span>
                              <span>
                                {row.site_details?.address?.country},
                                {row.site_details?.address?.pin_code}
                              </span>
                            </div>
                          ) : (
                            <div>
                              <span>
                                {row.siteData.address?.street}{' '}
                                {row.siteData.address?.city},{' '}
                                {row.siteData.address?.state},
                              </span>
                              <span>
                                {row.siteData.address?.country},
                                {row.siteData.address?.pin_code}
                              </span>
                            </div>
                          )}
                        </td>
                        <td>
                          <div className={Styles.statusProject}>
                            <span>Not Started</span>
                          </div>
                        </td>
                        <td>
                          <span>{row?.estimated_budget}</span>
                        </td>
                        <td>
                          <span>{row.actual_budget}</span>
                        </td>
                        <td>
                          <span>
                            {row.approvar_data?.first_name +
                              ' ' +
                              row.approvar_data?.last_name}
                          </span>
                        </td>
                        <td>
                          <div className={Styles.iconStyle}>
                            <NewEditIcon
                              onClick={(e) =>
                                handleProjectSiteEdit(row?.project_site_id)
                              }
                            />
                            <DeleteIcon />
                          </div>
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
            <div>
              <CustomPagination
                currentPage={currentPage}
                totalPages={initialData?.total_page}
                totalCount={initialData?.total_count}
                rowsPerPage={rowsPerPage}
                onPageChange={handlePageChange}
                onRowsPerPageChange={handleRowsPerPageChange}
              />
            </div>
          </div>) : (<div>
            <div className={Styles.subHeading}>
              < SiteNavigateIcon width={30} height={30} />
              <h3>SITES</h3>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div className={Styles.image}>
                <img
                  src="/siteAdd.png"
                  width="70%"
                  height="20%"
                />
              </div>
              <div>
                <h5 className={Styles.textmax}>No sites added to this Project</h5>
              </div>
              <div>
                <p className={Styles.textmin}>Go ahead, add a site from existing list or create new</p>
              </div>
              <div className={Styles.siteCreateButton}>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon color="white" />}
                    onClick={() => {
                      setMode('ADD');
                      setProjectSiteOpen(true);
                    }}
                  >
                    Add Site to Project
                  </Button>
                </div>
                <div
                  className={Styles.siteCreatelabelAdd}
                  onClick={() => {
                    setOpen(true);
                  }}
                >
                  <SiteNavigateIcon width={15} height={15} color="#7f56d9" />
                  <span className={Styles.sitelabel}>Create New Site</span>
                </div>
              </div>
            </div>
          </div>)}
        {/* <CustomSiteAdd /> */}
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
        <CustomSidePopup
          open={open}
          title="Create Site"
          handleClose={handleCloseSiteAdd}
          content={<CustomSiteAdd open={open} setOpen={setOpen} />}
        />
        <CustomSidePopup
          open={projectSiteOpen}
          title="Create New Project Site"
          handleClose={handleCloseProjectSite}
          content={
            <ProjectSiteConfigAdd
              open={projectSiteOpen}
              setOpen={setProjectSiteOpen}
              projectID={Number(routeParams?.id)}
              projectData={projectData}
              siteConfigData={siteConfigData}
              reload={reload}
              setReload={setReload}
              openSnack={openSnack}
              setOpenSnack={setOpenSnack}
              message={message}
              setMessage={setMessage}
              projectSiteId={projectSiteId}
              mode={mode}
            />
          }
        />
      </CustomLoader>
    </div>
  );
};

export default ProjectSiteConfig;
