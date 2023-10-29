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
import CustomSidePopup from '../ui/CustomSidePopup';
import ProjectTaskAdd from './forms/ProjectTaskAdd';
import PlanList from '../projectBOQ/planList';
import CheckListIcon from '../menu/icons/checkListIcon';
import EditIcon from '../menu/icons/newEditIcon';
import NewAddCircleIcon from '../menu/icons/newAddCircleIcon';
import ExpandIcon from '../menu/icons/expandIcon';
import CategoryService from '../../service/category-service';
import CustomMenu from '../ui/NewCustomMenu';
import subCategoryService from '../../service/subCategory-service';
import SubBoqItems from './subBoqItems';
import SettingIcon from '../menu/icons/settingIcon';


const BomItems = (props: {
  selectedCategory: any;
  setSelectedSubCategory: any;
  selectedSubCategory: any;
  projectsId: any;
  selectedBomConfig: any;
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
  const [categoryData, setCategoryData] = useState<any>();
  const [selectedSubCategoryId, setSelectedSubCategoryId] = useState();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [isWarning, setIswarning] = useState(false);
  const [mode, setMode] = useState('');
  const [reload, setReload] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [value, setValue] = useState();
  const [subTaskView, setSubTaskView] = useState(false);
  const navigate = useNavigate();
  const [isOpen, setIsOpen] = useState(false);
  const [moreIconDropdownOpen, setMoreIconDropdownOpen] = useState(false);
  const [openedContextMenuForSubCategory, setOpenedContextMenuForSubCategory] =
    useState<number | null>(null);
  const [subChildList, setSubChildList] = useState<any>([]);
  const primary_color = '#7f56d';
  const handleEdit = (value: any) => {
    setMode('EDIT');
    setShowSubCategoryForm(true);
    setSelectedSubCategoryId(value);
  };
  const handleSubTask = (value: any) => {
    setMode('Sub Task');
    setShowSubCategoryForm(true);
    setSelectedSubCategoryId(value);
  };
  const handleSubTaskView = async (value: any) => {
    setSubTaskView(!subTaskView);
    setSelectedSubCategoryId(value);
    const getSubChildList =
      await subCategoryService.getOneChlidSubCatListbyParentID(value);
    setSubChildList(getSubChildList?.data);
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
    <div className={Styles.task_page_container}>
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
                  // selectedSubCategoryId(!selectedSubCategoryId);
                  setShowSubCategoryForm(true);
                  setMode('Add');
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
                  {/* <th className={Styles.tableHeading}>Task Name</th> */}
                  <th className={Styles.tableHeading}>Task Description</th>
                  <th className={Styles.tableHeading}>Unit</th>
                  <th className={Styles.tableHeading}>Quantity</th>
                  <th className={Styles.tableHeading}>Rate</th>
                  <th className={Styles.tableHeading}>Amount</th>
                  <th className={Styles.tableHeading}>Action</th>
                </tr>
              </thead>
              <tbody>
                {getAllData?.map((data: any, index: number) => {
                  const subChildLength = data?.children?.length;
                  const actions = [
                    {
                      label: 'Manage Plans',
                      onClick: () => {
                        setSelectedSubCategoryId(data.sub_category_id);
                        setPlanListTitle(data.name);
                        setShowPlanForm(true);
                      },
                      disabled: subChildLength > 0 ? true : false,
                    },
                    {
                      label: 'Edit Task',
                      onClick: () => {
                        handleEdit(data?.sub_category_id);
                        setSelectedSubCategoryId(data?.sub_category_id);
                      },
                    },

                    {
                      label: 'Add sub Task',
                      onClick: () => {
                        handleSubTask(data?.sub_category_id);
                        setSelectedSubCategoryId(data?.sub_category_id);
                      },
                      disabled: data?.is_bom_detail === true,
                    },
                  ];
                  return (
                    <>
                      <tr
                        key={data.sub_category_id}
                        className={
                          selectedSubCategoryId == data?.sub_category_id
                            ? Styles.selectedRow
                            : ''
                        }
                        
                      >
                        <td
                          // onClick={(e) =>
                          //   handleSubTaskView(data.sub_category_id)
                          // }
                        >
                          {index + 1}
                        </td>
                        <td
                          onClick={(e) =>{
                            handleSubTaskView(data.sub_category_id)
                          }}
                          className={Styles.td_desc}
                        >
                          <span style={{ textAlign: 'justify' ,cursor: data?.children.length ? 'pointer':''}}>
                            {data.description || '--'}
                          </span>
                        </td>
                        <td>
                          <span style={{ textAlign: 'justify' }}>
                            {data?.uom_data?.name || '--'}
                          </span>
                        </td>
                        <td>
                          <span style={{ textAlign: 'justify' }}>
                            {data.quantity || '--'}
                          </span>
                        </td>
                        <td>
                          <span style={{ textAlign: 'justify' }}>
                            {data.rate || '--'}
                          </span>
                        </td>
                        <td>
                          <span style={{ textAlign: 'justify' }}>
                          {data?.estimated_budget ?formatBudgetValue(
                            data?.estimated_budget ? data?.estimated_budget : 0
                          ):'--'}
                          </span>
                        </td>


                        {/* <td
                          onClick={(e) =>
                            handleSubTaskView(data.sub_category_id)
                          }
                        >
                          {formatBudgetValue(
                            data?.actual_budget ? data?.actual_budget : 0
                          )}
                        </td> */}
                        <td >

                        <div className={Styles.actionIcons_container}>
                        <span style={{cursor: 'pointer'}}><EditIcon
                            onClick={() =>{  
                            handleEdit(data?.sub_category_id);
                            setSelectedSubCategoryId(data?.sub_category_id);}}
                            /></span>
                          
                          <span
                          onClick={()=>{    
                            handleSubTask(data?.sub_category_id);
                              setSelectedSubCategoryId(data?.sub_category_id);}}
                          >
                          <AddIcon width={20} height={20} color={primary_color} style={{cursor: 'pointer'}} />
                          </span>
                          {
                            data?.children?.length===0 ? (
                              <span
                          onClick={()=>{
                            setSelectedSubCategoryId(data.sub_category_id);
                            setPlanListTitle(data.name);
                            setShowPlanForm(true);
                          }}
                          >
                            <SettingIcon
                            style={{cursor: 'pointer'}}
                            color={primary_color}
                            />
                          </span>
                            ): (
                              <span
                                onClick={
                                  (e) =>{
                                    handleSubTaskView(data.sub_category_id)
                                  }
                                }
                                style={{ textAlign: 'justify' ,cursor: data?.children.length ? 'pointer':''}}
                                >
                                  <ExpandIcon
                                    color={primary_color}
                                    style={{fill_opacity : data?.children.length?'':'.5'}}
                                  ></ExpandIcon>
                                </span>
                            )
                          }
                           {/* <span>  <CustomMenu actions={actions} name="BoQItems" /></span> */}
                        </div>
                        
                         
                          {/* <span
                            className={Styles.menuText}
                            onClick={toggleMenu}
                          >
                            <MoreVerticalIcon />
                          </span>
                          {isOpen && (
                            <div className={Styles.menuDropdownItems}>

                            </div>
                          )} */}
                        </td>
                      </tr>
                      {!subTaskView &&
                        selectedSubCategoryId == data?.sub_category_id &&
                        subChildList?.map((item: any, subindex: any) => {
                          return (
                              <SubBoqItems
                              key={subindex}
                              index={subindex}
                              primaryIndex={index}
                              rowData={item}
                              reload={reload}
                              setReload={setReload}
                              subTaskView={subTaskView}
                              setSubTaskView={setSubTaskView} 
                              actions={undefined}                              />
                          );
                        })}
                    </>
                  );
                })}
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
                    setMode('Add');
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
            setAbstractReload={props.setReload}
            abstractReload={props.reload}
          />
        }
      />
    </div>
  );
};
export default BomItems;
