import React, { useEffect, useState } from 'react';
import {
  getBycategoryIdInSub,
  useDeleteSubcategory,
} from '../../hooks/subCategory-hooks';
import Styles from '../../styles/newStyles/bomlist.module.scss';
import AddIcon from '../menu/icons/addIcon';
import { formatBudgetValue } from '../../helper/common-function';
import { useNavigate } from 'react-router-dom';
import CustomSnackBar from '../ui/customSnackBar';
import CustomDelete from '../ui/customDeleteDialogBox';
import Button from '../ui/Button';
import CustomLoader from '../ui/customLoader';
import MoreVerticalIcon from '../menu/icons/moreVerticalIcon';
import CustomSidePopup from '../ui/CustomSidePopup';
import ProjectTaskAdd from './forms/ProjectTaskAdd';
import PlanList from '../projectBOQ/planList';
import CheckListIcon from '../menu/icons/checkListIcon';
import NewAddCircleIcon from '../menu/icons/newAddCircleIcon';
import CategoryService from '../../service/category-service';
import CustomMenu from '../ui/NewCustomMenu';

const BomItems = (props: {
  selectedCategory: any;
  setSelectedSubCategory: any;
  selectedSubCategory: any;
  projectsId: any;
  selectedBomConfig: any;
  setReload: any;
  reload: any;
}) => {
  const { selectedCategory, selectedBomConfig } = props;

  const obj = {
    selectedCategory: selectedCategory,
    selectedBomConfig: selectedBomConfig,
  };
  const { data: getAllData, refetch } = getBycategoryIdInSub(obj);
  const { mutate: getDeleteSubCategoryByID } = useDeleteSubcategory();
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [planListTitle, setPlanListTitle] = useState('');
  const [showPlanForm, setShowPlanForm] = useState(false);
  const [categoryData, setCategoryData] = useState();
  const [selectedSubCategoryId, setSelectedSubCategoryId] = useState();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [isWarning, setIswarning] = useState(false);
  const [mode, setMode] = useState('');
  const [reload, setReload] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [value, setValue] = useState();
  const navigate = useNavigate();
  const [moreIconDropdownOpen, setMoreIconDropdownOpen] = useState(false);
  const [openedContextMenuForSubCategory, setOpenedContextMenuForSubCategory] =
    useState<number | null>(null);

  const handleEdit = (value: any) => {
    setMode('EDIT');
    setShowSubCategoryForm(true);
    setSelectedSubCategoryId(value);
  };

  const deleteHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };

  const handleDelete = () => {
    getDeleteSubCategoryByID(value, {
      onSuccess: (response) => {
        const { message, data, status } = response;
        if (status === false) {
          setMessage('Task cannot be deleted');
          setOpenSnack(true);
          setIswarning(true);
          handleCloseDelete();
        } else {
          setMessage('Successfully deleted');
          setOpenSnack(true);
          handleCloseDelete();
        }
      },
    });
  };

  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  useEffect(() => {
    const closeContextMenu = () => {
      setMoreIconDropdownOpen(false);

      setOpenedContextMenuForSubCategory(null);
    };

    window.addEventListener('click', closeContextMenu);
    return () => {
      window.removeEventListener('click', closeContextMenu);
    };
  }, []);

  const handleCloseTask = () => {
    setShowSubCategoryForm(false);
  };
  const handleClosePlanList = () => {
    setShowPlanForm(false);
  };

  useEffect(() => {
    const fetchOne = async () => {
      const data = await CategoryService.getOneCategoryByID(
        Number(selectedCategory)
      );
      setCategoryData(data?.data);
    };
    refetch();
    fetchOne();
  }, [selectedCategory, reload]);

  return (
    <div>
      {getAllData ? (
        <div>
          <div className={Styles.mainHeading}>
            <div className={Styles.mainLeftContent}>
              <div className={Styles.leftContentOne}>
                <CheckListIcon />
                <h3 title={categoryData?.name}>
                  {categoryData?.name
                    ? categoryData?.name?.length > 20
                      ? categoryData?.name?.substring(0, 20) + '...'
                      : categoryData?.name
                    : '-'}
                  ({getAllData?.length})
                </h3>
              </div>
              <div
                className={Styles.leftContentOne}
                onClick={() => {
                  setShowSubCategoryForm(true);
                }}
              >
                <NewAddCircleIcon />
                <span className={Styles.menuFont}>Add Task</span>
              </div>
            </div>
            <div>
              <h3>
                {formatBudgetValue(
                  categoryData?.budget ? categoryData?.budget : 0
                )}
              </h3>
              <p className={Styles.countContentTitle}>Aggregated Value</p>
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <table className={Styles.boqSubCategoryTable}>
              <thead>
                <tr>
                  <th className={Styles.tableHeading}>#</th>
                  <th className={Styles.tableHeading}>Task Name</th>
                  <th className={Styles.tableHeading}>Task Description</th>
                  <th className={Styles.tableHeading}>Amount</th>
                  <th className={Styles.tableHeading}>Action</th>
                </tr>
              </thead>
              <tbody>
                {getAllData?.map((data: any, index: number) => {
                  const actions = [
                    {
                      label: 'Manage Plans',
                      onClick: () => {
                        setSelectedSubCategoryId(
                          data.sub_category_id
                        );
                        setPlanListTitle(data.name);
                        setShowPlanForm(true);
                      }
                    },
                    {
                      label: 'Edit Task',
                      onClick: () => {
                        handleEdit(data?.sub_category_id);
                        setSelectedSubCategoryId(
                          data?.sub_category_id
                        );
                      }
                    },

                  ];
                  return (
                  <tr key={data.sub_category_id}>
                    <td>{index + 1}</td>

                    <td>
                      <span title={data?.name}>
                        {data.name
                          ? data.name.length > 20
                            ? data.name.substring(0, 20) + '...'
                            : data.name
                          : '-'}
                      </span>
                    </td>

                    <td>
                      <span title={data?.description}>
                        {data.description
                          ? data.description.length > 20
                            ? data.description.substring(0, 20) + '...'
                            : data.description
                          : '-'}
                      </span>
                    </td>

                    <td>
                      {formatBudgetValue(data?.budget ? data?.budget : 0)}
                    </td>

                    <td>
                      <CustomMenu actions={actions} name="BoQItems" />
                      {/* <MoreVerticalIcon
                        onClick={(e: any) => {
                          e.stopPropagation();

                          setOpenedContextMenuForSubCategory(
                            data.sub_category_id
                          );

                          setMoreIconDropdownOpen(!moreIconDropdownOpen);
                        }}
                      /> */}

                      {/* {moreIconDropdownOpen &&
                        data.sub_category_id ===
                          openedContextMenuForSubCategory && (
                          <ul className={Styles.menu}>
                            <li className={Styles.menuItem}>
                              <div
                                style={{
                                  display: 'flex',
                                  flexDirection: 'column',
                                  gap: '5px',
                                  padding: '5px',
                                  backgroundColor: 'rgb(255, 255, 255)',
                                }}
                              >
                                <div
                                  className={Styles.options}
                                  onClick={() => {
                                    setSelectedSubCategoryId(
                                      data.sub_category_id
                                    );
                                    setPlanListTitle(data.name);
                                    setShowPlanForm(true);
                                    //   navigate(`/bom/${data?.sub_category_id}`);
                                  }}
                                >
                                  <span className={Styles.menuFont}>
                                    Manage Plan
                                  </span>
                                </div>
                              </div>
                            </li>
                            <li className={Styles.menuItem}>
                              <div
                                style={{
                                  display: 'flex',
                                  flexDirection: 'column',
                                  gap: '5px',
                                  padding: '5px',
                                  backgroundColor: 'rgb(255, 255, 255)',
                                }}
                              >
                                <div
                                  className={Styles.options}
                                  onClick={() => {
                                    handleEdit(data?.sub_category_id);
                                    setSelectedSubCategoryId(
                                      data?.sub_category_id
                                    );
                                  }}
                                >
                                  <span className={Styles.menuFont}>
                                    Edit Task
                                  </span>
                                </div>
                              </div>
                            </li>
                          </ul>
                        )} */}
                    </td>
                  </tr>
                )})}
              </tbody>
            </table>
          </div>

          <CustomSidePopup
            open={showPlanForm}
            title={planListTitle}
            width="85%"
            handleClose={handleClosePlanList}
            content={
              <PlanList
                open={showPlanForm}
                setOpen={setShowPlanForm}
                subCategoryId={selectedSubCategoryId}
                reload={reload}
                setReload={setReload}
                setAbstractReload={props.setReload}
                abstractReload={props.reload}
              />
            }
          />
          <CustomDelete
            open={openDelete}
            title="Delete Task"
            contentLine1="Are you sure you want to delete this Task ?"
            contentLine2=""
            handleClose={handleCloseDelete}
            handleConfirm={handleDelete}
          />

          <CustomSnackBar
            open={openSnack}
            message={message}
            onClose={handleSnackBarClose}
            autoHideDuration={1000}
            type={isWarning === true ? 'error' : 'success'}
          />
        </div>
      ) : (
        <div>
          <div className={Styles.Secondcontainer}>
            <div className={Styles.secondContainerContent}>
              <div>
                <CheckListIcon width={50} height={50} />
              </div>
              <div>
                <h5>No Tasks added to this Abstract</h5>
              </div>
              <div>
                <span>Let's add a task now</span>
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  size="small"
                  icon={<AddIcon width={20} color="white" />}
                  onClick={() => {
                    setShowSubCategoryForm(true);
                  }}
                >
                  Add Task
                </Button>
              </div>
            </div>
          </div>
        </div>
      )}
      <CustomSidePopup
        open={showSubCategoryForm}
        title={mode === 'EDIT' ? 'Edit Task' : 'Create New Task'}
        handleClose={handleCloseTask}
        content={
          <ProjectTaskAdd
            open={showSubCategoryForm}
            setOpen={setShowSubCategoryForm}
            selectedProject={props.projectsId}
            selectedBomConfig={props.selectedBomConfig}
            reload={reload}
            setReload={setReload}
            openSnack={openSnack}
            setOpenSnack={setOpenSnack}
            message={message}
            setMessage={setMessage}
            mode={mode}
            selectedCategoryId={selectedCategory}
            selectedSubCategory={selectedSubCategoryId}
          />
        }
      />
    </div>
  );
};
export default BomItems;
