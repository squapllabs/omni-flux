import React, { useState, useEffect } from 'react';
import Styles from '../../../styles/projectBomConfig.module.scss';

import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import Input from '../../ui/Input';
import Select from '../../ui/selectNew';
import {
  getBymasertDataType,
  getBymasertDataTypeDrop,
  getByMasterDataProjectIdDrop,
} from '../../../hooks/masertData-hook';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import { useFormik } from 'formik';
import * as yup from 'yup';
import DeleteIcon from '../../menu/icons/deleteIcon';
import {
  createProject,
  getByProjectId,
  useGetMasterProjectParentType,
  updateProject,
} from '../../../hooks/project-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import CustomSnackBar from '../../ui/customSnackBar';
import projectService from '../../../service/project-service';
import BOQIcon from '../../menu/icons/boqIcon';
import CustomLoader from '../../ui/customLoader';
import Pagination from '../../menu/CustomPagination';
import CustomPopup from '../../ui/CustomSidePopup';
import ProjectBoqAddPopup from './projectBoqAddPopup';
import { getBySearchBoQProject } from '../../../hooks/bom-hooks';
import CustomMenu from '../../ui/NewCustomMenu';

const ProjectBomConfig: React.FC = (props: any) => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const [initialValues, setInitialValues] = useState({
    bom_name: '',
    bom_description: '',
    bom_type_id: '',
    bom_type_name: '',
    budget: 0,
    is_delete: 'N',
    bom_configuration_id: '',
  });
  const [bomConfig, setBomConfig] = useState<any>([]);
  const [openConfirm, setOpenConfirm] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [projectData, setProjectData] = useState<any>({});
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [reload, setReload] = useState(false);
  const [open, setOpen] = useState(false);
  const [mode, setMode] = useState('');
  const [boqId, setBoQId] = useState();
  const [isLoading, setIsLoading] = useState(false);
  const action = [];
  // const { data: getBomType = [] } = getBymasertDataTypeDrop('BOMTP');
  const { data: getBomType = [] } = getByMasterDataProjectIdDrop(
    Number(routeParams?.id)
  );
  const { data: projectDatas } = getByProjectId(Number(routeParams?.id));

  const truncateString = (str: any, maxLength: number) => {
    if (projectDatas?.project_name.length > maxLength) {
      return str.substring(0, maxLength - 3) + '...';
    }
    return projectDatas?.project_name;
  };

  const truncatedString = truncateString(projectDatas?.project_name, 30);

  const { mutate: createNewProjectData } = createProject();
  const { mutate: updateProjectData } = updateProject();
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  const object: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    project_id: Number(routeParams?.id),
    global_search: '',
  };

  const {
    isLoading: getAllLoadingBoQProjectData,
    data: initialData,
    refetch,
  } = getBySearchBoQProject(object);

  // console.log("initialData", initialData);

  const handleAddBoQData = () => {
    setOpen(true);
    setMode('ADD');
  };
  const handleEdit = (value: any) => {
    setBoQId(value);
    setOpen(true);
    setMode('EDIT');
  };
  useEffect(() => {
    const fetchData = async () => {
      const getData = await projectService.getOneProjectById(
        Number(routeParams?.id)
      );
      setProjectData(getData?.data);
      let arr: any = [];
      const bomConfigurationRows = getData?.data?.bom_configuration.map(
        (items: any) => {
          const matchingObjects = getBomType.filter(
            (obj: any) => Number(obj.value) === Number(items?.bom_type_id)
          );
          let obj = {
            bom_name: items?.bom_name,
            bom_description: items?.bom_description,
            bom_type_id: items?.bom_type_id,
            bom_type_name: items?.bom_type_data?.master_data_name,
            budget: items?.budget,
            is_delete: `${items?.is_delete === false ? 'N' : 'Y'}`,
            bom_configuration_id: items?.bom_configuration_id,
          };
          arr.push(obj);
        }
      );
      setBomConfig(arr);
    };
    if (routeParams?.id != undefined) fetchData();
  }, []);
  // const validateSchema = yup.object().shape({
  //   bom_name: yup.string().required('BOM name is required'),
  //   bom_description: yup.string().required('BOM description is required'),
  //   bom_type_id: yup
  //     .string()
  //     .required('BOM type is required')
  //     .test(
  //       'decimal-validation',
  //       'Already exist',
  //       async function (value: number, { parent }: yup.TestContext) {
  //         let isDelete = parent.is_delete;
  //         try {
  //           const isValuePresent = bomConfig.some((obj: any) => {
  //             return (
  //               Number(obj.bom_type_id) === Number(value) &&
  //               obj.is_delete === isDelete
  //             );
  //           });
  //           if (isValuePresent === false) {
  //             return true;
  //           } else return false;
  //         } catch {
  //           return true;
  //         }
  //       }
  //     ),
  // });
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleClosePopup = () => {
    setOpen(false);
  };

  useEffect(() => {
    refetch();
  }, [rowsPerPage, currentPage]);

  /* Function for changing the table page */
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  /* Function for changing no of rows in pagination */
  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
  const handleSubmit = () => {
    const obj: any = {
      ...projectData,
      bom_configuration: bomConfig,
      site_configuration: projectData?.project_site,
    };
    if (routeParams?.id === undefined) {
      createNewProjectData(obj, {
        onSuccess: (data, variables, context) => {
          if (data?.status === true) {
            setMessage('BOM created');
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
            setMessage('BOM Updated');
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
  const formik = useFormik({
    initialValues,
    // validationSchema: validateSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      let arr = [];
      arr = [...bomConfig, values];
      setBomConfig(arr);
      resetForm();
    },
  });
  return (
    <div>
      <div className={Styles.container}>
        <CustomLoader
          loading={isLoading ? isLoading : getAllLoadingBoQProjectData}
          size={48}
          color="#333C44"
        >
          {initialData?.total_count !== 0 ? (
            <div>
              <div className={Styles.topHeading}>
                <div className={Styles.heading}>
                  <div className={Styles.subHeading}>
                    <BOQIcon />
                    <h3>{`List of BoQ (${initialData?.total_count})`}</h3>
                  </div>
                  <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      icon={<AddIcon color="white" />}
                      onClick={() => handleAddBoQData()}
                    >
                      Add BoQ
                    </Button>
                  </div>
                </div>
                {/* <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div> */}
              </div>
              {/* <div className={Styles.box}> */}
                <div className={Styles.tableContainerBOM}>
                  <div>
                    <table className={Styles.scrollable_table}>
                      <thead>
                        <tr>
                          <th>#</th>
                          {/* <th>BoQ Name</th> */}
                          <th>Description</th>
                          <th>Type</th>
                          <th>Abstracts</th>
                          <th>Tasks</th>
                          {/* <th>Plans</th> */}
                          <th>Actions</th>
                          {/* {activeButton === 'AC' && <th>Action</th>} */}
                        </tr>
                      </thead>
                      <tbody>
                        {initialData?.total_count === 0 ? (
                          <tr>
                            <td colSpan="8" style={{ textAlign: 'center' }}>
                              No data found
                            </td>
                            {/* {activeButton === 'AC' && <td></td>} */}
                          </tr>
                        ) : (
                          initialData?.content?.map(
                            (data: any, index: number) => {
                              const actions = [
                                {
                                  label: 'Manage Abstracts, Tasks or Plans',
                                  onClick: () => {
                                    navigate(
                                      `/newBoq/${routeParams?.id}/${data?.bom_configuration_id}`
                                    );
                                  },
                                },
                                {
                                  label: 'Edit BoQ Name & Description',
                                  onClick: () => {
                                    handleEdit(data?.bom_configuration_id);
                                  },
                                },
                              ];
                              return (
                                <tr key={data?.bom_configuration_id}>
                                  <td>{startingIndex + index}</td>
                                  {/* <td>{data?.bom_name}</td> */}
                                  <td
                                  onClick={()=>{
                                    console.log('initialData',initialData)
                                    console.log('data',data)
                                  }}
                                  >{data?.bom_description}
                                  
                                  </td>
                                  <td>
                                    {data?.bom_type_data?.master_data_name}
                                  </td>
                                  <td>{data?.abstract_count}</td>
                                  <td>{data?.task_count}</td>
                                  {/* <td>{0}</td> */}
                                  <td>
                                    <CustomMenu actions={actions} />
                                  </td>
                                </tr>
                              );
                            }
                          )
                        )}
                        <td></td>
                      </tbody>
                    </table>
                  </div>
                  <div>
                    <Pagination
                      currentPage={currentPage}
                      totalPages={
                        // dataShow
                        //   ? getFilterData?.total_page
                        //   :
                        initialData?.total_page
                      }
                      totalCount={
                        // dataShow
                        //   ? getFilterData?.total_count
                        //   :
                        initialData?.total_count
                      }
                      rowsPerPage={rowsPerPage}
                      onPageChange={handlePageChange}
                      onRowsPerPageChange={handleRowsPerPageChange}
                    />
                  </div>
                </div>
              {/* </div> */}
            </div>
          ) : (
            <div>
              <div className={Styles.subHeading}>
                <BOQIcon />
                <h3>BoQ</h3>
              </div>
              <div className={Styles.emptyDataHandling}>
                <div className={Styles.imageAdd}>
                  <img src="/boq-add.png" alt="aa" width="100%" height="150px" />
                </div>
                <div>
                  <h5 className={Styles.textmax}>
                    No BoQ added to this Project
                  </h5>
                </div>
                <div>
                  <p className={Styles.textmin}>Go ahead, add a BoQ </p>
                </div>
                <div className={Styles.emptyButton}>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon color="white" />}
                    onClick={() => handleAddBoQData()}
                  >
                    Add BoQ
                  </Button>
                </div>
              </div>
            </div>
          )}
        </CustomLoader>
      </div>
      <CustomPopup
        title={
          mode === 'ADD'
            ? `Add BoQ to ${truncatedString}`
            : `Edit BoQ to ${truncatedString}`
        }
        open={open}
        handleClose={handleClosePopup}
        content={
          <ProjectBoqAddPopup
            setOpen={setOpen}
            open={open}
            mode={mode}
            boqId={boqId}
            setReload={setReload}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
            projectId={Number(routeParams?.id)}
          />
        }
      />
    </div>
  );
};

export default ProjectBomConfig;
