import React, { useEffect, useRef, useState } from 'react';
import {
  useGetAllCategory,
  useGetAllCategoryByProjectId,
  useDeleteCategory,
} from '../../hooks/category-hooks';
import { read, utils, writeFile } from 'xlsx';
import Styles from '../../styles/newStyles/bomlist.module.scss';
import MoreVerticalIcon from '../menu/icons/moreVerticalIcon';
import subCategoryService from '../../service/subCategory-service';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import NewAddCircleIcon from '../menu/icons/newAddCircleIcon';
import CustomLoader from '../ui/customLoader';
import CustomBomAddPopup from '../ui/CustomBomAddPopup';
import CustomAbstractAddPopup from '../ui/CustomAbstractPopup';
import CustomSubCategoryAddPopup from '../ui/CustomSubCategoryPopup';
import BomService from '../../service/bom-service';
import { formatBudgetValue } from '../../helper/common-function';
import BomItems from '../projectBOQ/boqItems';
// import Bom from './bom';
import { useNavigate, useParams } from 'react-router-dom';
import CategoryService from '../../service/category-service';
import EditIcon from '../menu/icons/newEditIcon';
import ViewIcon from '../menu/icons/newViewIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import CustomSnackBar from '../ui/customSnackBar';
import CustomDelete from '../ui/customDeleteDialogBox';
import { getByProjectId } from '../../hooks/project-hooks';
import BackArrow from '../menu/icons/backArrow';
import ZIcon from '../menu/icons/zIcon';
import CheckListIcon from '../menu/icons/checkListIcon';
import CustomSidePopup from '../ui/CustomSidePopup';
import ProjectAbstractAdd from './forms/projectAbstractAdd';
import ProjectTaskAdd from './forms/ProjectTaskAdd';
import CustomMenu from '../ui/NewCustomMenu';
import FileUploadIcon from '../menu/icons/fileUploadIcon';
import { createMultipleCategory, getBySearchCategroy} from '../../hooks/category-hooks';
import Pagination from '../menu/CustomPagination';
const temp : React.FC = ()=>{
  return (
    <div></div>
  )
}



const BomList: React.FC = (props: any) => {
  const { mutate: createMultipleNewCategory } = createMultipleCategory();
  // const { mutate : getBySearchNewCategory } = getBySearchCategroy();
  const params = useParams();
  const navigate = useNavigate();
  const projectId = Number(params?.projectId);
  const bomconfigId = Number(params?.bomconfigId);
  const { data: projectData } = getByProjectId(projectId);
  const [projectsId, setProjectsId] = useState(projectId);
  const [selectedCategory, setSelectedCategory] = useState();
  const [selectedSubCategory, setSelectedSubCategory] = useState();
  const [showItemForm, setShowItemForm] = useState(false);
  const [showAbstractForm, setShowAbstractForm] = useState(false);
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [categoryData, setCategoryData] = useState();
  const [moreIconDropdownOpen, setMoreIconDropdownOpen] = useState(false);
  const [openedContextMenuForCategory, setOpenedContextMenuForCategory] =
    useState<number | null>(null);
  const [categoryId, setCategoryId] = useState();
  const [categories, setCategories] = useState();
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [open, setOpen] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [isWarning, setIswarning] = React.useState(false);
  const { mutate: getDeleteCategoryByID } = useDeleteCategory();
  const [value, setValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [isloading, setIsloading] = useState(true);
  const [abstractPopup, setAbstractPopup] = useState(false);
  const [abstractBulkData, setAbstractBulkData] = useState({});
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [initialData, setInitialData] = useState(null);
  const [overallBudget, setOverallBudget] = useState<string>('')
  useEffect(() => {
    const fetchData = async () => {
      const obj = {
        projectId: projectId,
        bomconfigId: bomconfigId,
      };
      const datas = await CategoryService.getAllCategoryByProjectId(obj);
      setCategories(datas.data);
      calculateOverallBudget(datas.data)
      setIsloading(false);
      setCategoryData(datas.data[0]);
      setSelectedCategory(datas.data[0].category_id);
      setCategoryId(datas.data[0].category_id);
    };

    fetchData();
  }, [reload]);

  const calculateOverallBudget = (abstracts:any)=>{
    if(abstracts && abstracts.length){
      let total_count = 0;
      abstracts.forEach((element:any) => {
        total_count = total_count + Number(element.estimated_budget)
      });
      console.log('total_count',total_count);
      const formattedValue = total_count.toLocaleString('en-IN', {
        style: 'currency',
        currency: 'INR',
        minimumFractionDigits: 2,
      });
      setOverallBudget(formattedValue)
      return false
    }
    setOverallBudget('')
    return false
  }
  
  // const object: any = {
  //   offset: (currentPage - 1) * rowsPerPage,
  //   limit: rowsPerPage,
  //   order_by_column: 'updated_date',
  //   order_by_direction: 'desc',
  //   status: 'AC',
  //   search_by_name:''
  // };
  // getBySearchNewCategory(object,{
  //   onSuccess:(data: any)=>{
  //     console.log('data',data)
  //     setInitialData(data)
  //   }
  // })
  // const {
  //   isLoading: getAllLoadingBoQProjectData,
  //   data: initialData,
  //   refetch,
  // } = getBySearchCategroy(object);

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


  const handleSelectedCategory = async (value: any) => {
    setSelectedCategory(value.category_id);
    setCategoryData(value);
    const subCatList = await subCategoryService.getOneSubCatListbyCatID(
      Number(value.category_id)
    );
    if (subCatList?.message === 'success') {
      setOpen(!open);
      setReload(true);
    } else {
      setOpen(false);
    }
  };

  const handleEdit = (value: any) => {
    setMode('EDIT');
    setCategoryId(value);
    setShowAbstractForm(true);
  };

  const deleteHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
const handleFileUploadBtnClick = () =>{
  if (fileInputRef.current) {
    fileInputRef.current.click();
  }
}
  const handleDelete = () => {
    getDeleteCategoryByID(value, {
      onSuccess: (response) => {
        const { message, data, status } = response;
        if (status === false) {
          setMessage('Abstract cannot be deleted');
          setOpenSnack(true);
          setIswarning(true);
          handleCloseDelete();
        } else {
          setMessage('Abstract deleted');
          setOpenSnack(true);
          setReload(true);
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
      setOpenedContextMenuForCategory(null);
    };
    window.addEventListener('click', closeContextMenu);
    return () => {
      window.removeEventListener('click', closeContextMenu);
    };
  }, []);

  const handleCloseAbstract = () => {
    setShowAbstractForm(false);
  };

  const handleCloseTask = () => {
    setShowSubCategoryForm(false);
  };
  // const open = true;
const handleFileOnChange = async (e:any) =>{
  const file = e.target.files[0];
  if (file) {
    const reader = new FileReader();
    reader.onload = (e:any) => {
      const data = e.target.result;
      const workbook = read(data, { type: 'array' });
      const sheet = workbook.Sheets[workbook.SheetNames[0]];
      const jsonData = utils.sheet_to_json(sheet);
      const parsedJson = JSON.parse(JSON.stringify(jsonData));
  

      if(parsedJson.length > 1){
        let  bulkUploadData = {}
        const header = parsedJson.shift();
        bulkUploadData = {
          abstractName : header['Name_of_work']
        }
        const bodyData = parsedJson.filter((element:any) => {
          const keys = Object.keys(element)
          if(keys.length >= 2){
            return true
          }
  
          return false
        });
        if(bodyData.length){
          bulkUploadData = {
            ...bulkUploadData,
            bodyData
          }
          setAbstractBulkData(bulkUploadData)
          setAbstractPopup(true)
          // uploadBulkAbstract(bulkUploadData)
        }
      }
    
      // Update the state with the Excel data
      // setExcelData(jsonData);
    };
    reader.readAsArrayBuffer(file);
  }
  }


  const uploadBulkAbstract = (data:any) =>{
    if(data && data?.bodyData.length){
      console.log('data?.bodyData',data?.bodyData)
      const abstractData: { name: any; description: any; estimated_budget: any; project_id: any; budget: number; start_date: string; end_date: string; bom_configuration_id: any; progress_status: string; }[] = []

      data.bodyData.forEach((element:any)=>{
        const obj = {
          name : data.abstractName || '',
          description: element['Name_of_work'],
          estimated_budget: element['Amount'],
          project_id: projectsId,
          budget: 0,
          start_date: '',
          end_date: '',
          bom_configuration_id: bomconfigId,
          progress_status: 'Inprogress',
        }

        abstractData.push(obj)
      })
      console.log('abstractData',abstractData)
      createMultipleNewCategory(abstractData,{
        onSuccess: (data, variables, context) => {
          props.setMessage('Abstracts created');
          // props.setOpenSnack(true);
          props.setReload(!props.reload);
          handleClosePopup();
          if (data?.status === true) {
           
            // resetForm();
          }
        },
      })

    }else {
      console.log('error')
    }
  }
  const handleClosePopup = ():void => {
    setAbstractPopup(false)
    console.log('')
  }
  return (
    <div>
      <div>
      <CustomSidePopup
          open={abstractPopup}
          title={"Abstract List"}
          handleClose={handleClosePopup}
          content={<div>
            <table
            style={{padding: '20px'}}
            >
                            <thead>
                              <tr
                               style={{padding: '20px 0'}}
                              >
                                <th>S No</th>
                                <th>Name of Work</th>
                                <th>Amount</th>
                              </tr>
                            </thead>
                            <tbody>
                              {abstractBulkData && abstractBulkData?.bodyData?.length > 0 ? (
                                abstractBulkData?.bodyData.map((item: any, index: any) => (
                                  <tr key={index}
                                  
                                  >
                                    <td
                                    style={{padding: '10px 0'}}
                                    >{index + 1}</td>
                                    <td
                                    style={{padding: '10px 0'}}
                                    >{item['Name_of_work']}</td>
                                    <td
                                    style={{padding: '10px 0'}}
                                    >{item['Amount']}</td>
                                  </tr>
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
                     onClick={() => {
                       uploadBulkAbstract(abstractBulkData);
                       handleClosePopup()
                     }}
                     justify='center'
                   >
                     Add Abstracts
                   </Button>
           </div>
          </div>
           
        
        } width={'70%'} description={"description"}/>

         
      </div>

      {isloading ? (
        <CustomLoader loading={isloading} size={48} />
      ) : (
        <div className={Styles.container}>
          {categories ? (
            <div className={Styles.abstract_container}>
              <div className={'Styles'}>
              </div>
              {/* Header Container */}
              <div className={`${Styles.header_container} ${Styles.flex} ${Styles.space_between}`}>
                <div className={`${Styles.flex} ${Styles.gap_3}`}>
                <div className={`${Styles.flex}`}>
                    <ZIcon />
                    <h3>Abstracts ({categories?.length})</h3>
                </div>
                <div className={`${Styles.flex} ${Styles.add_abstract_container}`}
                onClick={() => {
                                setShowAbstractForm(true);
                                setMode('Add');
                              }}
                >
                  <NewAddCircleIcon onClick={function (): void {
                        throw new Error('Function not implemented.');
                      } } />
                  <span className={Styles.menuFont}>Add Abstract</span>                      
                </div>
                <div className={`${Styles.flex} ${Styles.bulkUpload_container}`}
                onClick={handleFileUploadBtnClick}
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
                          color="#7f56d9" 
                          />
                  </span>
                  <span className={Styles.menuFont}>Bulk Upload</span>                      
                </div>
                </div>
                
                
                <div>
              <h3>
                {/* {formatBudgetValue(
                  categoryData?.budget ? categoryData?.budget : 0
                )} */}
                { overallBudget || '0.00'}
              </h3>
              <p className={Styles.countContentTitle}>Aggregated Value</p>
            </div>

            </div>
                 {/* Table Container */}
                <div className={Styles.ab_tableContainer}>
                  <table className={Styles.boqSubCategoryTable}>
                    <thead>
                      <tr>
                        <th>#</th>
                        {/* <th>Task Name</th> */}
                        <th>Description</th>
                        <th>Status</th>
                        <th>Amount</th>
                        <th>Action</th>
                      </tr>
                    </thead>
                    <tbody>
                      {categories?.map((data: any, index: number) => {
                        const actions = [
                                        {
                                          label: 'Edit Abstract',
                                          onClick: () => {
                                            handleEdit(items?.category_id);
                                          },
                                        },
                                        // {
                                        //   label: 'Delete',
                                        //   onClick: () => { deleteHandler(items.category_id) }
                                        // },
                                      ];
                        return (
                            <tr key={data.category_id}
                            >
                              <td
                                // onClick={(e) =>
                                //   handleSubTaskView(data.sub_category_id)
                                // }
                              >
                                {index + 1}
                              </td>
                              <td
                              onClick={
                                (e) =>{
                                  // console.log("data",data)
                                  console.log("categories",categories)
                                }
                              }
                              
                              >
                                {data.description || '--'}
                              </td>
                              <td>
                                {data.progress_status || '--'}
                              </td>
                              <td>
                                {data.estimated_budget || '--'}
                              </td>
                              <td >
                              <div className={Styles.actionIcons_container}>
                              <span style={{cursor: 'pointer'}}><EditIcon
                                  onClick={() =>{  
                                    handleEdit(data?.category_id);
                                    }
                                  }
                                  /></span>

                                  <span style={{cursor: 'pointer'}}><ViewIcon
                                    onClick={() =>{  
                                    navigate(
                                      `/newBoq/${projectId}/${bomconfigId}/${data?.category_id}`
                                    )
                                    }
                                    }
                                  /></span>
                              </div>
                              {/* <div className={Styles.actionIcons_container}>
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
                                  ): ('')
                                }
                              </div> */}
                              </td>
                            </tr>
                        );
                      })}
                      <td></td>
                    </tbody>
                  </table>
                </div>
                <div>
                    {/* <Pagination
                      currentPage={currentPage}
                      totalPages={
                        initialData?.total_page
                      }
                      totalCount={
                        initialData?.total_count
                      }
                      rowsPerPage={rowsPerPage}
                      onPageChange={handlePageChange}
                      onRowsPerPageChange={handleRowsPerPageChange}
                    /> */}
                  </div>
            </div>
            
            // <div className={Styles.subHeader}>
            //   <div className={Styles.subcontainer}>
            //     <div className={Styles.submenu}>
            //       <div className={Styles.side_menu}>
            //         <div className={Styles.topSideMenu}>
            //           <div className={Styles.topSideHeading}>
            //             
            //             <h3>Abstracts ({categories?.length})</h3>
            //           </div>
            //           <span>
            //           <input
            //                   ref={fileInputRef}
            //                   id="upload-photo"
            //                   name="upload_photo"
            //                   type="file"
            //                   style={{ display: 'none' }}
            //                   onChange={(e) => handleFileOnChange(e)}
            //                 />
            //           <FileUploadIcon
            //                 width={20}
            //                 height={20}
            //                 color="#7f56d9" onClick={handleFileUploadBtnClick}
            //                 />
            //           </span>
            //           <NewAddCircleIcon
            //             onClick={() => {
            //               setShowAbstractForm(true);
            //               setMode('Add');
            //             }}
            //           />
            //         </div>
            //         <div>
                    
            //           {categories?.map((items: any, index: any) => {
            //             // console.log('categories', categories);
            //             const actions = [
            //               {
            //                 label: 'Edit Abstract',
            //                 onClick: () => {
            //                   handleEdit(items?.category_id);
            //                 },
            //               },
            //               // {
            //               //   label: 'Delete',
            //               //   onClick: () => { deleteHandler(items.category_id) }
            //               // },
            //             ];

            //             return (
            //               <div>
            //                 <ul key={index}>
            //                   <li
            //                     className={
            //                       selectedCategory === items.category_id
            //                         ? Styles.selected
            //                         : ''
            //                     }
            //                     onClick={() => {
            //                       handleSelectedCategory(items);
            //                       setCategoryId(items.category_id);
            //                     }}
            //                   >
            //                     {/* it is category shows */}
            //                     <div
            //                       style={{
            //                         display: 'flex',
            //                         justifyContent: 'space-between',
            //                       }}
            //                     >
            //                       <div>
            //                         {index + 1} {items?.name}
            //                         <span className={Styles.smallred}>
            //                           {items?.progress_status}
            //                         </span>
            //                         <div className={Styles?.subMenuDescription}>
            //                           <span>{items?.description}</span>
            //                         </div>
            //                       </div>
            //                       <div>
            //                         <CustomMenu
            //                           actions={actions}
            //                           name={'abstract'}
            //                         />
            //                         {/* <MoreVerticalIcon
            //                       onClick={(e: any) => {
            //                         e.stopPropagation();
            //                         setOpenedContextMenuForCategory(
            //                           items.category_id
            //                         );
            //                         setCategoryId(items.category_id);
            //                         setMoreIconDropdownOpen(
            //                           !moreIconDropdownOpen
            //                         );
            //                       }}
            //                       color="#7f56d9"
            //                     /> */}
            //                         {/* {moreIconDropdownOpen &&
            //                       items.category_id ===
            //                       openedContextMenuForCategory && (
            //                         <ul className={Styles.menu}>
            //                           <li className={Styles.menuItem}>
            //                             <div
            //                               style={{
            //                                 display: 'flex',
            //                                 flexDirection: 'column',
            //                                 gap: '5px',
            //                                 padding: '5px',
            //                               }}
            //                             >
            //                               <div
            //                                 className={Styles.options}
            //                                 onClick={() =>
            //                                   handleEdit(items.category_id)
            //                                 }
            //                               >
            //                                 <span
            //                                   className={Styles.menuFont}
            //                                 >
            //                                   Edit Abstract
            //                                 </span>
            //                               </div>
            //                               <div
            //                                 className={Styles.options}
            //                                 onClick={() =>
            //                                   deleteHandler(
            //                                     items.category_id
            //                                   )
            //                                 }
            //                               >
            //                                 <span
            //                                   className={Styles.menuFont}
            //                                 >
            //                                   Delete
            //                                 </span>
            //                               </div>
            //                             </div>
            //                           </li>
            //                         </ul>
            //                       )} */}
            //                       </div>
            //                     </div>
            //                   </li>
            //                 </ul>
            //               </div>
            //             );
            //           })}
            //         </div>
            //       </div>
            //     </div>
            //     <div className={Styles.mainContainer}>
            //       <BomItems
            //         selectedCategory={selectedCategory}
            //         setSelectedSubCategory={setSelectedSubCategory}
            //         selectedSubCategory={selectedSubCategory}
            //         projectsId={projectsId}
            //         selectedBomConfig={bomconfigId}
            //       />
            //     </div>
            //   </div>
              
            // </div>
          ) : (
            <div className={Styles.Secondcontainer}>
              <div className={Styles.secondContainerHeading}>
                <ZIcon />
                <span>Abstract (0)</span>
              </div>
              <div className={Styles.secondContainerContent}>
                <div>
                  <ZIcon width={50} height={50} />
                </div>
                <div>
                  <h5>No Abstracts added to this BoQ</h5>
                </div>
                <div>
                  <span>
                    Every BoQ needs an Abstract to begin with. Go ahead, add an
                    Abstract now.
                  </span>
                </div>
                <div className={Styles.bulkUpload_add_btn_container}>
                  <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    size="small"
                    icon={<AddIcon width={20} color="white" />}
                    onClick={() => {
                      setShowAbstractForm(true);
                    }}
                  >
                    Add Abstract
                  </Button>
                  </div>
                  <div>
                      <input
                              ref={fileInputRef}
                              id="upload-photo"
                              name="upload_photo"
                              type="file"
                              style={{ display: 'none' }}
                              onChange={(e) => handleFileOnChange(e)}
                            />
                            <Button
                              color="primary"
                              shape="rectangle"
                              size="small"
                              icon={<FileUploadIcon width={20} height={15} color="white" onClick={function (): void {
                                console.log('')
                              } } />}
                              onClick={handleFileUploadBtnClick}
                            >
                              Bulk Upload
                            </Button>
                               
                      </div>
                   

                </div>
              </div>
            </div>
          )}
        </div>
      )}
      <CustomBomAddPopup isVissible={showItemForm} onAction={setShowItemForm} />
      <CustomSidePopup
        open={showAbstractForm}
        title={mode === 'EDIT' ? 'Edit Abstract' : 'Create New Abstract'}
        handleClose={handleCloseAbstract}
        content={
          <ProjectAbstractAdd
            open={showAbstractForm}
            setOpen={setShowAbstractForm}
            selectedProject={projectsId}
            selectedBomConfig={bomconfigId}
            reload={reload}
            setReload={setReload}
            openSnack={openSnack}
            setOpenSnack={setOpenSnack}
            message={message}
            setMessage={setMessage}
            mode={mode}
            categoryId={categoryId}
          />
        }
      />
      <CustomSidePopup
        open={showSubCategoryForm}
        title={mode === 'EDIT' ? 'Edit Task' : 'Create New Task'}
        handleClose={handleCloseTask}
        content={
          <ProjectTaskAdd
            open={showSubCategoryForm}
            setOpen={setShowSubCategoryForm}
            selectedProject={projectsId}
            selectedBomConfig={bomconfigId}
            reload={reload}
            setReload={setReload}
            openSnack={openSnack}
            setOpenSnack={setOpenSnack}
            message={message}
            setMessage={setMessage}
            mode={mode}
            selectedCategoryId={categoryId}
          />
        }
      />
      <CustomDelete
        open={openDelete}
        title="Delete Abstract"
        contentLine1="Are you sure you want to delete this Abstract"
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
  );
};

export default BomList;
