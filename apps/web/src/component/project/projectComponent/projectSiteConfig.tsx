import React, { useEffect, useState } from 'react';
import Button from '../../ui/Button';
import { useNavigate, useParams } from 'react-router-dom';
import CustomSnackBar from '../../ui/customSnackBar';
import Styles from '../../../styles/projectSiteConfig.module.scss';
import DeleteIcon from '../../menu/icons/newDeleteIcon';
import CustomSiteAdd from '../../ui/CustomSiteAdd';
import AddIcon from '../../menu/icons/addIcon';
import SiteNavigateIcon from '../../menu/icons/siteNavigateIcon';
import NewEditIcon from '../../menu/icons/newEditIcon';
import CustomSidePopup from '../../ui/CustomSidePopup';
import ProjectSiteConfigAdd from './projectSiteConfigAdd';
import {
  useGetAllPaginatedprojectSite,
} from '../../../hooks/projectSite-hooks';
import CustomPagination from '../../menu/CustomPagination';
import CustomLoader from '../../ui/customLoader';
import { formatBudgetValue } from '../../../helper/common-function';
/* Site list screen for a project */
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
  const [modalOpen, setModalOpen] = useState(false);
  const masterData = {
    limit: 10,
    offset: 0,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    project_id: Number(routeParams?.id),
    global_search: '',
  };
  /* Function to get all sites of a project */
  const {
    data: initialData,
    refetch,
    isLoading: getAllLoadingProjectMasterData,
  } = useGetAllPaginatedprojectSite(masterData);
  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, reload]);
  const handleCloseSiteAdd = () => {
    setOpen(false);
    setModalOpen(false)
  };
  const handleCloseProjectSite = () => {
    setProjectSiteOpen(false);
    setModalOpen(false);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const handleProjectSiteEdit = (value: any) => {
    setMode('EDIT');
    setProjectSiteId(value);
    setProjectSiteOpen(true);
    setModalOpen(true);
  };
  /* Function to change page */
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };
  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  useEffect(() => {
    if (modalOpen === true) {
      document.body.style.overflow = 'hidden';
    } else {
      document.body.style.overflow = 'auto';
    }
  }, [modalOpen]);

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
                  <h3>SITES</h3>
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
                      setModalOpen(true);
                    }}
                  >
                    Add Site to Project
                  </Button>
                </div>
                <div
                  className={Styles.siteCreatelabel}
                  onClick={() => {
                    setOpen(true);
                    setModalOpen(true)
                  }}
                >
                  <SiteNavigateIcon width={15} height={15} color="#7f56d9" />
                  <span className={Styles.sitelabel}>Create New Site</span>
                </div>
              </div>
            </div>

            <div className={Styles.tableContainer}>
              <div>
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
                    {initialData?.content.map((row: any, index: number) => {
                      rowIndex = rowIndex + 1;
                      return (
                        <tr key={index}>
                          <td>{rowIndex}</td>
                          <td>{row.site_details?.name}</td>
                          <td>
                            {row?.siteData?.site_contractor_id === undefined ? (
                              <div>
                                <span>
                                  {row?.site_details?.address?.street || row?.site_details?.address?.city || row?.site_details?.address?.state || row?.site_details?.address?.country || row?.site_details?.address?.pin_code
                                    ? `${row?.site_details?.address?.street} ${row?.site_details?.address?.city} ${row?.site_details?.address?.state}`
                                    : '-'
                                  }
                                </span>
                                <span>
                                  {row?.site_details?.address?.country || row?.site_details?.address?.pin_code
                                    ? `${row?.site_details?.address?.country} ${row?.site_details?.address?.pin_code}`
                                    : ''
                                  }
                                </span>
                              </div>

                            ) : (
                              <div>
                                <span>
                                  {row?.siteData?.address?.street}{' '}
                                  {row?.siteData?.address?.city}{' '}
                                  {row?.siteData?.address?.state}
                                </span>
                                <span>
                                  {row.siteData.address?.country}
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
                            <span>{formatBudgetValue(row?.estimated_budget ? row?.estimated_budget : 0)}</span>
                          </td>
                          <td>
                            <span>{formatBudgetValue(row?.actual_budget ? row?.actual_budget : 0)}</span>
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
          </div>
        ) : (
          <div>
            <div className={Styles.subHeadingForInitialPage}>
              <SiteNavigateIcon width={30} height={30} />
              <h3>SITES</h3>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div className={Styles.image}>
                <img src="/siteAdd.png" alt="sites" width="70%" height="20%" />
              </div>
              <div>
                <h5 className={Styles.textmax}>
                  No sites added to this Project
                </h5>
              </div>
              <div>
                <p className={Styles.textmin}>
                  Go ahead, add a site from existing list or create new
                </p>
              </div>
              <div className={Styles.siteCreateButton}>
                <div className={Styles.emptyButton}>
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
          </div>
        )}
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
          content={<CustomSiteAdd open={open} setOpen={setOpen} setModalOpen={setModalOpen} />}
        />
        <CustomSidePopup
          open={projectSiteOpen}
          title={
            mode === 'EDIT' ? 'Edit Project Site' : 'Create New Project Site'
          }
          handleClose={handleCloseProjectSite}
          description={'Create a new project site'}
          content={
            <ProjectSiteConfigAdd
              open={projectSiteOpen}
              setOpen={setProjectSiteOpen}
              setModalOpen={setModalOpen}
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
