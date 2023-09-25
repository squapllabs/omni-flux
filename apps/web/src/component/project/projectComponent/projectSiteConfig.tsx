import React, { ChangeEvent, useEffect, useState } from 'react';
import Button from '../../ui/Button';
import { useNavigate, useParams } from 'react-router-dom';
import CustomSnackBar from '../../ui/customSnackBar';
import projectService from '../../../service/project-service';
import Styles from '../../../styles/project.module.scss';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import { useFormik } from 'formik';
import * as yup from 'yup';
import DeleteIcon from '../../menu/icons/deleteIcon';
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
  const [siteConfigData, setSiteConfigData] = useState<any[]>([]);
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
          };
          arr.push(obj);
        }
      );
      setSiteConfigData(arr);
    };
    if (routeParams?.id != undefined) fetchData();
  }, []);
  const handelOpenSiteForm = () => {
    setShowSiteForm(true);
  };
  const handleChangeNewRowItems = async (
    event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
    index: number
  ) => {
    const { name, value } = event.target;
    const updatedSiteConfigData = [...siteConfigData];
    updatedSiteConfigData[index][name] = value;

    // Update the address when a site is selected
    if (name === 'site_id') {
      const siteId = value;
      const siteData = await siteService.getOneSiteById(siteId);
      updatedSiteConfigData[index].address = siteData?.data?.address;
    }

    setSiteConfigData(updatedSiteConfigData);
  };
  const addEmptyRow = () => {
    setSiteConfigData([...siteConfigData, {}]); // Add an empty object
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
      <div className={Styles.tableContainer}>
        <div>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>S No</th>
                <th className={Styles.tableHeadingSite}>Site</th>
                <th className={Styles.tableHeading}>Site Address</th>
                <th className={Styles.tableHeading}>Status</th>
                <th className={Styles.tableHeading}>Estimated Budget</th>
                <th className={Styles.tableHeading}>Actual Budget</th>
                <th className={Styles.tableHeading}>Approver</th>
                <th className={Styles.tableHeading}>Action</th>
              </tr>
            </thead>
            <tbody>
              {siteConfigData.map((row, index) => {
                // console.log('row', row);

                rowIndex = rowIndex + 1;
                return (
                  <tr key={index}>
                    <td>{rowIndex}</td>
                    <td>
                      <div className={Styles.selectedProjectName}>
                        <div className={Styles.siteField}>
                          <AutoCompleteSelect
                            name="site_id"
                            width="200px"
                            defaultLabel="Select Site"
                            placeholder="Select from options"
                            value={row?.site_id}
                            onChange={(e) => handleChangeNewRowItems(e, index)} // Use handleChangeNewRowItems here
                            onSelect={(value) => {
                              handleChangeNewRowItems(
                                { target: { name: 'site_id', value } },
                                index
                              ); // Use handleChangeNewRowItems here
                            }}
                            optionList={getAllSite}
                          />
                        </div>
                      </div>
                    </td>
                    <td>
                      {row?.siteData?.site_contractor_id === undefined ? (
                        <div>
                          <span>
                            {row.address?.street} {row.address?.city},{' '}
                            {row.address?.state},
                          </span>
                          <span>
                            {row.address?.country},{row.address?.pin_code}
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
                      <div className={Styles.siteEstimation}>
                        <Input
                          width="140px"
                          placeholder="Enter estimation"
                          name="estimated_budget"
                          onChange={(e) => handleChangeExistItems(e, index)}
                          value={row?.estimated_budget}
                        />
                      </div>
                    </td>
                    <td>
                      <div className={Styles.siteEstimation}>
                        <Input
                          width="140px"
                          placeholder="Enter actual budget"
                          name="actual_budget"
                          onChange={(e) => handleChangeExistItems(e, index)}
                          value={row.actual_budget}
                        />
                      </div>
                    </td>
                    <td>
                      <div className={Styles.siteEstimation}>
                        <AutoCompleteSelect
                          name="approvar_id"
                          value={row?.approvar_id}
                          defaultLabel="Select Approver"
                          placeholder="Select from options"
                          onSelect={(value) => {
                            setValue({ ...value, ['approvar_id']: value });
                          }}
                          onChange={(e) => handleChangeExistItems(e, index)}
                          optionList={getProjectApproverList}
                        />
                      </div>
                    </td>
                    <td>
                      <div className={Styles.actionIcon}>
                        <div
                          className={Styles.addPlan}
                          onClick={() => {
                            navigate(
                              `/expenses/${routeParams.id}/${row?.site_id}`
                            );
                            // navigate(
                            //   `/bomlist/${routeParams.id}/${row?.bom_configuration_id}`
                            // );
                          }}
                          style={{
                            pointerEvents:
                              `${row?.project_site_id}` === ''
                                ? 'none'
                                : 'auto',
                          }}
                        >
                          <p className={Styles.addText}> + Add Site Expense</p>
                        </div>
                      </div>
                    </td>
                  </tr>
                );
              })}
              {/* <tr>
                <td>{rowIndex + 1}</td>
                <td>
                  <div className={Styles.selectedProjectName}>
                    <div className={Styles.siteField}>
                      <AutoCompleteSelect
                        width="200px"
                        name="site_id"
                        defaultLabel="Select Site"
                        placeholder="Select from options"
                        value={value.site_id}
                        onSelect={(datas) => {
                          setValue((prevValue: any) => {
                            const updatedValue = {
                              ...prevValue,
                              site_id: datas,
                            };
                            addressSet(updatedValue.site_id);
                            return updatedValue;
                          });
                        }}
                        error={errors?.site_id}
                        optionList={getAllSite}
                      />
                    </div>
                    <div
                      className={Styles.instantAdd}
                      onClick={handelOpenSiteForm}
                    >
                      <AddIcon style={{ height: '15px', width: '15px' }} />
                      <h4 className={Styles.addtext}> Add Site</h4>
                    </div>
                  </div>
                </td>
                <td>
                  {' '}
                  {value.site_id && (
                    <div>
                      <span>
                        {viewAddress.address?.street}{' '}
                        {viewAddress.address?.city},{' '}
                        {viewAddress.address?.state},
                      </span>
                      <span>
                        {viewAddress.address?.country},
                        {viewAddress.address?.pin_code}
                      </span>
                    </div>
                  )}
                </td>
                <td>
                  <div className={Styles.statusProject}>
                    {value.site_id && <span>Not Started</span>}
                  </div>
                </td>

                <td>
                  {value.site_id ? (
                    <div className={Styles.siteEstimation}>
                      <Input
                        width="140px"
                        placeholder="Enter budget"
                        name="estimated_budget"
                        onChange={handleChangeItems}
                        value={value.estimated_budget}
                        error={errors?.estimated_budget}
                      />
                    </div>
                  ) : (
                    ''
                  )}
                </td>
                <td>
                  {value.site_id ? (
                    <div className={Styles.siteEstimation}>
                      <Input
                        width="140px"
                        placeholder="Enter budget"
                        name="actual_budget"
                        onChange={handleChangeItems}
                        value={value.actual_budget}
                        error={errors?.actual_budget}
                      />
                    </div>
                  ) : (
                    ''
                  )}
                </td>
                <td>
                  {value.site_id ? (
                    <div className={Styles.siteEstimation}>
                      <AutoCompleteSelect
                        name="approvar_id"
                        defaultLabel="Select Approver"
                        placeholder="Select from options"
                        value={value.approvar_id}
                        onSelect={(datas) => {
                          setValue({ ...value, ['approvar_id']: datas });
                        }}
                        optionList={getProjectApproverList}
                        error={errors?.approvar_id}
                      />
                    </div>
                  ) : (
                    ''
                  )}
                </td>
                <td>
                  <div
                    style={{
                      cursor: 'pointer',
                      paddingBottom: '20px',
                    }}
                  >
                    <div onClick={(e) => addRow(e)}>
                      <AddIcon />
                    </div>
                  </div>
                </td>
              </tr> */}
            </tbody>
          </table>
          <div className={Styles.buttonContent}>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
              icon={<AddIcon color="white" />}
              onClick={addEmptyRow}
            >
              Add Site
            </Button>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
              icon={<AddIcon color="white" />}
              onClick={(e) => {
                handleSubmit(e);
              }}
            >
              SITE CONFIG
            </Button>
          </div>
        </div>
      </div>
      <CustomSiteAdd
        isVissiblesite={showSiteForm}
        onActionsite={setShowSiteForm}
      />
    </div>
  );
};

export default ProjectSiteConfig;
