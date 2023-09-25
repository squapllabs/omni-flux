import React, { useState, useEffect } from 'react';
import Styles from '../../../styles/project.module.scss';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import Input from '../../ui/Input';
import Select from '../../ui/selectNew';
import {
  getBymasertDataType,
  getBymasertDataTypeDrop,
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
  const { data: getBomType = [] } = getBymasertDataTypeDrop('BOMTP');
  const { mutate: createNewProjectData } = createProject();
  const { mutate: updateProjectData } = updateProject();
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
  const validateSchema = yup.object().shape({
    bom_name: yup.string().required('BOM name is required'),
    bom_description: yup.string().required('BOM description is required'),
    bom_type_id: yup
      .string()
      .required('BOM type is required')
      .test(
        'decimal-validation',
        'Already exist',
        async function (value: number, { parent }: yup.TestContext) {
          let isDelete = parent.is_delete;
          try {
            const isValuePresent = bomConfig.some((obj: any) => {
              return (
                Number(obj.bom_type_id) === Number(value) &&
                obj.is_delete === isDelete
              );
            });
            if (isValuePresent === false) {
              return true;
            } else return false;
          } catch {
            return true;
          }
        }
      ),
  });
  const handleSnackBarClose = () => {
    setOpenSnack(false);
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
    validationSchema: validateSchema,
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
      <div className={Styles.tableContainer}>
        <div>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>S No</th>
                <th className={Styles.tableHeadingSite}>BOQ Name</th>
                <th className={Styles.tableHeading}>BOQ Description</th>
                <th className={Styles.tableHeading}>BOQ Type</th>
                <th className={Styles.tableHeading}>Budget</th>
                <th className={Styles.tableHeading}>Action</th>
              </tr>
            </thead>
            <tbody>
              {bomConfig?.map((item: any, index: any) => {
                rowIndex = rowIndex + 1;
                return (
                  // <>
                    <tr>
                      <td>{rowIndex}</td>
                      <td>{item?.bom_name}</td>
                      <td>{item?.bom_description}</td>
                      <td>{item?.bom_type_name}</td>
                      <td>{item?.budget}</td>
                      <td>
                        <div
                          className={Styles.addPlan}
                          onClick={() => {
                            navigate(
                              `/bomlist/${routeParams.id}/${item?.bom_configuration_id}`
                            );
                          }}
                          style={{
                            pointerEvents:
                              `${item?.bom_configuration_id}` === ''
                                ? 'none'
                                : 'auto',
                          }}
                        >
                          <AddIcon style={{ height: '15px', width: '15px' }} />
                          <p className={Styles.addText}>Add BOQ</p>
                        </div>
                      </td>
                    </tr>
                  // </>
                );
              })}
              <tr>
                <td>{rowIndex + 1}</td>
                <td>
                  <Input
                    name="bom_name"
                    value={formik.values.bom_name}
                    onChange={formik.handleChange}
                    error={formik.touched.bom_name && formik.errors.bom_name}
                  />
                </td>
                <td>
                  <Input
                    name="bom_description"
                    value={formik.values.bom_description}
                    onChange={formik.handleChange}
                    error={
                      formik.touched.bom_description &&
                      formik.errors.bom_description
                    }
                  />
                </td>
                <td>
                  <AutoCompleteSelect
                    defaultLabel=""
                    // width="250px"
                    name="bom_type_id"
                    mandatory={true}
                    optionList={getBomType}
                    value={formik.values.bom_type_id}
                    onChange={formik.handleChange}
                    onSelect={(value) => {
                      formik.setFieldValue('bom_type_id', value);
                      const matchingObjects = getBomType.filter(
                        (obj: any) => Number(obj.value) === Number(value)
                      );
                      formik.setFieldValue(
                        'bom_type_name',
                        matchingObjects[0]?.label
                      );
                    }}
                    error={
                      formik.touched.bom_type_id && formik.errors.bom_type_id
                    }
                  />
                </td>
                <td>
                  {' '}
                  <div
                    style={{
                      cursor: 'pointer',
                      paddingBottom: '20px',
                    }}
                  >
                    {formik.values.budget}
                  </div>
                </td>
                <td>
                  <div
                    style={{
                      cursor: 'pointer',
                      paddingBottom: '20px',
                    }}
                  >
                    <div onClick={formik.handleSubmit}>
                      <AddIcon />
                    </div>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
          <div className={Styles.buttonContent}>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
              onClick={(e) => {
                handleSubmit(e);
              }}
            >
              Save Config
            </Button>
          </div>
        </div>
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </div>
    </div>
  );
};

export default ProjectBomConfig;
