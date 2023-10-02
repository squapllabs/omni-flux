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

const ProjectSiteConfig: React.FC = (props: any) => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const [projectData, setProjectData] = useState<any>({});
  const valueObject: any = {
    site_id: '',
    estimated_budget: '',
    actual_budget: '',
    approvar_id: '',
    status: 'Not Started',
    is_delete: 'N',
    address: '',
  };
  const [value, setValue] = useState(valueObject);
  const [initialValues, setInitialValues] = useState({
    site_id: '',
    estimated_budget: '',
    actual_budget: '',
    approvar_id: '',
    status: 'Not Started',
    is_delete: 'N',
    address: '',
  });
  const [viewAddress, setViewAddress] = useState({});
  const [showSiteForm, setShowSiteForm] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [errors, setErrors] = useState('');
  const [open, setOpen] = useState(false);
  const [projectSiteOpen, setProjectSiteOpen] = useState(false);
  const [siteConfigData, setSiteConfigData] = useState<any[]>([]);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [projectSiteId, setProjectSiteId] = useState();
  const { data: getAllSite = [] } = useGetAllSiteDrops();
  const { data: getAllUsersDatadrop = [] } = useGetAllUsersDrop();
  const { mutate: createNewProjectData } = createProject();
  const { mutate: updateProjectData } = updateProject();
  let Obj: any = {
    projectID: Number(routeParams?.id),
    role: 'Site Engineer',
  };
  const { data: getProjectApproverList = [] } =
    getUserDataProjectRolebased(Obj);
  const addRow = async () => {
    setErrors({});
    const schema = yup.object().shape({
      site_id: yup
        .string()
        .trim()
        .required('Site is required')
        .test(
          'unique-site-ids',
          'Site name repeated are not allowed',
          function (sites: any) {
            const isSiteIdUnique = siteConfigData.every(
              (siteData) => siteData.site_id !== parseInt(sites, 10)
            );
            return isSiteIdUnique;
          }
        ),
      approvar_id: yup.string().trim().required('Approver is required'),
      actual_budget: yup
        .string()
        .matches(/^[0-9]*$/, 'Only numbers are allowed'),
      estimated_budget: yup
        .string()
        .matches(/^[0-9]*$/, 'Only numbers are allowed')
        .required('Budget is required')
        .typeError('Only numbesqswqsrs are allowed')
        .test(
          'site-budget',
          'Site budget is greater than estimated budget',
          async function (budget: any) {
            const estimated_budget = projectData.estimated_budget;
            const site_configuration = siteConfigData;
            if (site_configuration.length === 0) {
              if (Number(budget) > Number(estimated_budget)) {
                return false;
              }
              return true;
            } else {
              const totalEstimation = site_configuration.reduce(
                (total: any, site: any) =>
                  total + Number(site.estimated_budget),
                0
              );
              const finalTotal = totalEstimation + Number(budget);
              if (finalTotal > estimated_budget) {
                return false;
              }
              return true;
            }
          }
        ),
    });
    await schema
      .validate(value, { abortEarly: false })
      .then(async () => {
        const siteData = await siteService.getOneSiteById(value.site_id);
        value['address'] = siteData?.data?.address;
        const updatedObject = {
          ...value,
          site_id: Number(value.site_id),
          estimated_budget: Number(value.estimated_budget),
          approvar_id: Number(value.approvar_id),
          actual_budget: value.actual_budget ? Number(value.actual_budget) : 0,
        };
        setSiteConfigData([...siteConfigData, updatedObject]);
        setValue({
          site_id: '',
          estimated_budget: '',
          approvar_id: '',
          status: 'Not Started',
          is_delete: 'N',
        });
      })
      .catch((e) => {
        const errorObj: any = {};
        e.inner.map((errors: any) => {
          return (errorObj[errors.path] = errors.message);
        });
        setErrors({
          ...errorObj,
        });
      });
  };
  const addressSet = async (value: any) => {
    const siteId = value;
    const siteData = await siteService.getOneSiteById(siteId);
    setViewAddress(siteData?.data);
  };
  useEffect(() => {
    const fetchData = async () => {
      const getData = await projectService.getOneProjectById(
        Number(routeParams?.id)
      );
      setProjectData(getData?.data);
      console.log('getData?.data?.project_site', getData?.data?.project_site);
      let arr: any = [];
      const siteConfigurationRows = getData?.data?.project_site.map(
        (config: any) => {
          let obj = {
            site_id: config.site_id,
            siteData: config.site_details,
            estimated_budget: config.estimated_budget,
            actual_budget: config.actual_budget,
            approvar_id: config.approvar_id,
            status: config.status,
            project_site_id: config.project_site_id,
            ...config,
          };
          arr.push(obj);
        }
      );
      setSiteConfigData(getData?.data?.project_site);
    };
    if (routeParams?.id != undefined) fetchData();
  }, [reload]);
  const handelOpenSiteForm = () => {
    setShowSiteForm(true);
  };
  const handleCloseSiteAdd = () => {
    setOpen(false);
  };
  const handleCloseProjectSite = () => {
    setProjectSiteOpen(false);
  };
  const handleChangeItems = async (
    event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
  ) => {
    setValue({
      ...value,
      [event.target.name]: event.target.value,
    });

    if (event.target.name === 'site_id') {
      const siteId = event.target.value;
      const siteData = await siteService.getOneSiteById(siteId);
      setViewAddress(siteData?.data);
    }
  };
  const handleChangeExistItems = (
    event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
    index: number
  ) => {
    setValue({
      ...value,
      [event.target.name]: Number(event.target.value),
    });
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const handleProjectSiteEdit = (value: any) => {
    setMode('EDIT');
    setProjectSiteId(value);
    setProjectSiteOpen(true);
  };
  const handleSubmit = () => {
    const obj: any = {
      ...projectData,
      site_configuration: siteConfigData,
      bom_configuration: projectData?.bom_configuration,
    };
    if (routeParams?.id === undefined) {
      createNewProjectData(obj, {
        onSuccess: (data, variables, context) => {
          if (data?.status === true) {
            setMessage('Site created');
            setOpenSnack(true);
            props.setLoader(!props.loader);
            setTimeout(() => {
              navigate(`/project-edit/${data?.data?.project?.project_id}`);
              props.setLoader(props.loader);
              props.setActiveButton('PGS');
            }, 2000);
          }
        },
      });
    } else {
      updateProjectData(obj, {
        onSuccess: (data, variables, context) => {
          if (data?.status === true) {
            setMessage('Site Updated');
            setOpenSnack(true);
            props.setLoader(!props.loader);
            setTimeout(() => {
              navigate(`/project-edit/${data?.data?.project?.project_id}`);
              props.setLoader(props.loader);
              props.setActiveButton('PGS');
            }, 2000);
          }
        },
      });
    }
  };
  return (
    <div>
      <div className={Styles.topHeading}>
        <div className={Styles.heading}>
          <div className={Styles.subHeading}>
            <SiteNavigateIcon width={30} height={30} />
            <h4>SITE</h4>
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
            {siteConfigData.map((row, index) => {
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
    </div>
  );
};

export default ProjectSiteConfig;
