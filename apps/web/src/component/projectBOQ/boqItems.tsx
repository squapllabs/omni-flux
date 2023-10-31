import React, { useEffect, useRef, useState } from 'react';
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
import FileUploadIcon from '../menu/icons/fileUploadIcon';
import DownloadIcon from '../menu/icons/download';
import DeleteIcon from '../menu/icons/newDeleteIcon';
import { read, utils } from 'xlsx';
import CustomPopupModel from '../ui/CustomPopupModel';


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
  const [uploadedTaskData , setUploadedTaskData] = useState<any[]>([]);
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [subChildList, setSubChildList] = useState<any>([]);
  const [ TaskPopupTrigger , setTaskPopupTrigger] = useState(false)
  const [ modelPopupTrigger , setModelPopupTrigger] = useState(false)
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
  const handleFileOnChange = async (e:any) =>{
    const file = e.target.files[0];
    setModelPopupTrigger(false)
    if (file) {
      const reader = new FileReader();
      reader.onload = (e:any) => {
        const data = e.target.result;
        const workbook = read(data, { type: 'array' });
        const sheet = workbook.Sheets[workbook.SheetNames[0]];
        const jsonData = utils.sheet_to_json(sheet);
        const parsedJson = JSON.parse(JSON.stringify(jsonData));
        if(parsedJson.length > 1){

          const taskData: { id: string | undefined; description: any; uom_id: number; quantity: any; rate: any; estimated_budget: any; actual_budget: number; children: any[]; }[] = []
        const formattedTaskData = parsedJson.forEach((element:any) =>{
          
         const  s_no_array = String(element['SI.NO']).split('.');
          if(s_no_array.length === 1){
            const obj = {
              id : s_no_array.pop(),
              description : (element?.Description || ''),
              uom_id:0,
              quantity:(element?.Quantity || ''),
              rate: (element?.Rate|| ''),
              estimated_budget: (element?.Amount|| ''),
              actual_budget: 0,
              children : [],
              project_id: categoryData?.project_id,
              bom_configuration_id:categoryData?.bom_configuration_id,
              category_id:categoryData?.category_id,
            }
            taskData.push(obj);
          } else if(s_no_array.length === 2){
            const parentId = s_no_array.shift();
            const parenIndex = taskData.findIndex(parent => parent.id === parentId)
            const obj = {
              id : s_no_array.pop(),
              description : (element?.Description || ''),
              uom_id:0,
              quantity:(Number(element?.Quantity) || ''),
              rate: (Number(element?.Rate)|| ''),
              estimated_budget: (element?.Amount|| ''),
              actual_budget: 0,
              children : [],
              project_id: categoryData?.project_id,
              bom_configuration_id:categoryData?.bom_configuration_id,
              category_id:categoryData?.category_id,
            }
            taskData[parenIndex].children.push(obj)
          }
        });
        
        
        if(taskData.length){
          setUploadedTaskData(taskData)
          setTaskPopupTrigger(true)
          console.log('taskData',taskData) 
        }else {
          setUploadedTaskData([])
        }
          // const bulkUploadData = parsedJson.map((element:any) => ({
          //   description: element,
          //   project_id: props.selectedProject,
          //   actual_budget: 0,
          //   category_id: props.selectedCategoryId,
          //   start_date: '',
          //   end_date: '',
          //   bom_configuration_id: props.selectedBomConfig,
          //   parent_sub_category_id:
          //   props.mode === 'Sub Task' ? props.selectedSubCategory : null,
          //   estimated_budget : values.estimated_budget,
          //   uom_id : values.uom_id,
          //   rate :  values.rate,
          //   quantity : element
          // }));
          
        }else {
          // throw error message
        }
      
        // Update the state with the Excel data
        // setExcelData(jsonData);
      };
      reader.readAsArrayBuffer(file);
    }
    }
    const handleFileUploadBtnClick = () =>{
      setModelPopupTrigger(true)
    }

  

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

  const handleClosePopup = ():void => {
    setTaskPopupTrigger(false)
    console.log('')
  }
  const handleCloseModelPopup = ()=>{
    setModelPopupTrigger(false)
  }


  const handleTaskBulkUpload = ()=>{
    debugger
  }
 
  return ( 
    <div className={Styles.task_page_container}>

<div>
      <CustomSidePopup
          open={TaskPopupTrigger}
          title={categoryData?.name}
          handleClose={handleClosePopup}

          
          content={
          <div className={Styles.ab_tableContainer}>
            <table

            style={{padding: '20px',width:'100%'}}
            >
                            <thead>
                              <tr
                               style={{padding: '20px 0'}}
                              >
                                <th  style={{padding: '0 10px'}}>S No</th>
                                <th style={{padding: '0 10px'}}>Description</th>
                                <th style={{padding: '0 10px'}}>Uom</th>
                                <th style={{padding: '0 10px'}}>Quantity</th>
                                <th style={{padding: '0 10px'}}>Rate</th>
                                <th style={{padding: '0 10px'}}>Amount</th>
                                <th style={{padding: '0 10px'}}>Action</th>
                              </tr>
                            </thead>
                            <tbody>
                              {uploadedTaskData && uploadedTaskData?.length > 0 ? (
                                uploadedTaskData.map((item: any, index: any) => (
                                  <>
                                  <tr key={index}
                                  >
                                    <td
                                    style={{padding: '10px 10px'}}
                                    >{index + 1}</td>
                                    <td
                                    style={{padding: '10px 10px'}}
                                    >{item.description}</td>
                                     <td
                                    style={{padding: '10px 10px'}}
                                    >{('Unit'|| 0)}</td>
                                     <td
                                    style={{padding: '10px 10px'}}
                                    >{(item.quantity || 0)}</td>
                                    <td
                                    style={{padding: '10px 10px'}}
                                    >{(item.rate || 0)}</td>
                                    <td
                                    style={{padding: '10px 10px'}}
                                    >{(item.estimated_budget || 0)}</td>
                                    <td
                                    style={{padding: '10px 10px', cursor:'pointer'}}
                                    ><DeleteIcon 
                                    onClick={()=>{
                                      if(uploadedTaskData.length){
                                        const data = [...uploadedTaskData]
                                        data.splice(index,1);
                                        setUploadedTaskData(data)
                                      }else {
                                      console.log('throw error')
                                      }
                                    }}
                                    /></td>
                                    {/* <td
                                    style={{padding: '10px 10px'}}
                                    >{ (Number(item.estimated_budget) || 0) * }</td> */}
                                  </tr>
                                  {item?.children.length ?
                                  item.children.map((child: any, childIndex: any) =>(
                                    <tr key={childIndex}>
                                    <td
                                    style={{padding: '10px 10px'}}
                                    >{(index+1 )+ '.'+ (childIndex + 1)}</td>
                                    <td
                                    style={{padding: '10px 10px'}}
                                    >{child.description}</td>
                                     <td
                                    style={{padding: '10px 10px'}}
                                    >{('Unit'|| 0)}</td>
                                     <td
                                    style={{padding: '10px 10px'}}
                                    >{(child.quantity || 0)}</td>
                                    <td
                                    style={{padding: '10px 10px'}}
                                    >{(child.rate || 0)}</td>
                                    <td
                                    style={{padding: '10px 10px'}}
                                    >{(item.estimated_budget || 0)}</td>
                                    </tr>
                                  ))
                                  :('')}
                                  </>
                                ))
                              ) : (
                                <tr>
                                  <td></td>
                                  <td></td>
                                  <td>No records found</td>
                                  <td></td>
                                  <td></td>
                                </tr>
                              )}
                            </tbody>
                          </table>

                    <div
                    style={{
                      display:'flex',
                      justifyContent:'center',
                      padding:'20px'
                    }}
                    >

                    <Button
                     color="primary"
                     shape="rectangle"
                     size="small"
                     icon={<AddIcon width={20} color="white" />}
                     onClick={handleTaskBulkUpload}
                     justify='center'
                   >
                     Add Tasks
                   </Button>
           </div>
          </div> 
        } width={'90%'} description={"description"}/>

         <CustomPopupModel 
          open={modelPopupTrigger}
          title={categoryData?.name}
          handleClose={handleCloseModelPopup}
          content={<div className={`${Styles.flex} ${Styles.space_between}`}
          style={{display:'flex',justifyContent:'space-between',alignItems:'center',padding:'2rem'}}
          >
            <div>
            <Button
                  color="primary"
                  shape="rectangle"
                  size="small"
                  icon={<FileUploadIcon width={20} color="white" onClick={function (): void {
                    console.log('')
                  } } />}
                  onClick={() => {
                    if (fileInputRef.current) {
                      fileInputRef.current.click();
                    }              
                  }}
                >
                  Upload File
                </Button>
            </div>
            <div>
            <Button
                  color="primary"
                  shape="rectangle"
                  size="small"
                  icon={<DownloadIcon width={20} color="white" onClick={function (): void {
                    console.log('')
                  } } />}
                  onClick={() => {
                    if (fileInputRef.current) {
                      fileInputRef.current.click();
                    }              
                  }}
                >
                 Download Template
                </Button>
            </div>
          </div>}
          width={'50%'} description={''}         />
      </div>


      {getAllData ? (
        <div>
          <div className={Styles.mainHeading}>
            <div className={Styles.mainLeftContent}>
              <div className={Styles.leftContentOne}>
                <CheckListIcon 
                style={{padding:'4px 0 0 0;'}}
                />
                <h3 title={categoryData?.name}>
                  {/* {categoryData?.name
                    ? categoryData?.name?.length > 20
                      ? categoryData?.name?.substring(0, 20) + '...'
                      : categoryData?.name
                    : '-'} */}
                    {categoryData?.name}
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
              <div className={`${Styles.flex} ${Styles.gap_2} ${Styles.bulkUpload_container}`}
              onClick={handleFileUploadBtnClick}
              style={{cursor:'pointer'}}
              >
              <span>
                    <input
                            ref={fileInputRef}
                            id="upload-photo"
                            name="upload_photo"
                            type="file"
                            style={{ display: 'none' }}
                            onChange={(e) => handleFileOnChange(e)}
                          />
                    <FileUploadIcon
                    width={20}
                    height={20}
                    color="#7f56d9" onClick={function (): void {
                      throw new Error('Function not implemented.');
                    } }                          />
                  </span>
                  <span className={Styles.menuFont}>Bulk Upload</span>                      
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
