import React, { useEffect, useRef, useState } from 'react';
import {
  useGetBycategoryIdInSub,
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
import ExpandClose from '../menu/icons/expandClose';
import CategoryService from '../../service/category-service';
import subCategoryService from '../../service/subCategory-service';
import SubBoqItems from './subBoqItems';
import SettingIcon from '../menu/icons/settingIcon';
import FileUploadIcon from '../menu/icons/fileUploadIcon';
import DownloadIcon from '../menu/icons/download';
import DeleteIcon from '../menu/icons/newDeleteIcon';
import { read, utils } from 'xlsx';
import CustomPopupModel from '../ui/CustomPopupModel';
import { useCreateMultipleSubcategory } from '../../hooks/subCategory-hooks';
import { useGetUomByType } from '../../hooks/uom-hooks';
import CustomLoader from '../ui/customLoader';

const BomItems = (props: {
  selectedCategory: any;
  setSelectedSubCategory: any;
  selectedSubCategory: any;
  projectsId: any;
  selectedBomConfig: any;
}) => {
  const { selectedCategory, selectedBomConfig } = props;
  const { mutate: getDeleteSubCategoryByID } = useDeleteSubcategory();
  const { mutate: createNewMultipleSubcategory } =
    useCreateMultipleSubcategory();
  const obj = {
    selectedCategory: selectedCategory?.category_id,
    selectedBomConfig: selectedBomConfig,
  };
  const { data: getAllData, refetch } = useGetBycategoryIdInSub(obj);
  const { data: uomObject } = useGetUomByType('RAWMT');
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [planListTitle, setPlanListTitle] = useState('');
  const [showPlanForm, setShowPlanForm] = useState(false);
  const [categoryData, setCategoryData] = useState<any>();
  const [selectedSubCategoryId, setSelectedSubCategoryId] = useState<any>();
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
  const [uploadedTaskData, setUploadedTaskData] = useState<any[] | null>(null);
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [subChildList, setSubChildList] = useState<any>([]);
  const [TaskPopupTrigger, setTaskPopupTrigger] = useState(false);
  const [modelPopupTrigger, setModelPopupTrigger] = useState(false);
  const [isloading, setIsLoading] = useState(false);
  const [colps, setColps] = useState(false);
  const [totalAmount, setTotalAmount] = useState<any>(0);
  const [selectedSubCategory, setSelectedSubCategory] = useState<any>(null);
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
    if (subTaskView === true) {
      setSelectedSubCategoryId(value);
    } else {
      setSelectedSubCategoryId('');
    }
    const getSubChildList =
      await subCategoryService.getOneChlidSubCatListbyParentID(value);
    setSubChildList(getSubChildList?.data);
  };
  const deleteHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
  /* Function to delete a task from the list */
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
  const handleFileOnChange = async (e: any) => {
    const file = e.target.files[0];
    if (file) {
      const reader = new FileReader();
      reader.onload = (e: any) => {
        const data = e.target.result;
        const workbook = read(data, { type: 'array' });
        const sheet = workbook.Sheets[workbook.SheetNames[0]];
        const jsonData = utils.sheet_to_json(sheet);
        const parsedJson = JSON.parse(JSON.stringify(jsonData));
        if (parsedJson.length > 1) {
          const taskData: {
            id: string | undefined;
            description: any;
            uom_id: number;
            quantity: any;
            rate: any;
            estimated_budget: any;
            actual_budget: number;
            children: any[];
          }[] = [];
          parsedJson.forEach((element: any) => {
            const uom = uomObject
              .filter(
                (uomObj: any) =>
                  uomObj.label?.toLowerCase() === element?.Unit?.toLowerCase()
              )
              .shift();
            const s_no_array = String(element['SI.NO']).split('.');
            if (s_no_array.length === 1) {
              const obj = {
                id: s_no_array.pop(),
                description: element?.Description || '',
                uom_id: uom?.value || '',
                uom_label: uom?.label || '',
                quantity: element?.Quantity || '',
                rate: element?.Rate || '',
                estimated_budget: element?.Amount || '',
                actual_budget: 0,
                children: [],
                project_id: categoryData?.project_id,
                bom_configuration_id: categoryData?.bom_configuration_id,
                category_id: categoryData?.category_id,
              };
              taskData.push(obj);
            } else if (s_no_array.length === 2) {
              const parentId = s_no_array.shift();
              const parenIndex = taskData.findIndex(
                (parent) => parent.id === parentId
              );
              const obj = {
                id: s_no_array.pop(),
                description: element?.Description || '',
                uom_id: uom?.value || '',
                uom_label: uom?.label || '',
                quantity: Number(element?.Quantity) || '',
                rate: Number(element?.Rate) || '',
                estimated_budget: element?.Amount || '',
                actual_budget: 0,
                children: [],
                project_id: categoryData?.project_id,
                bom_configuration_id: categoryData?.bom_configuration_id,
                category_id: categoryData?.category_id,
              };
              taskData[parenIndex].children.push(obj);
            }
          });
          if (taskData.length) {
            setUploadedTaskData(taskData);
          } else {
            setUploadedTaskData([]);
          }
        } else {
          // throw error message
        }
      };
      reader.readAsArrayBuffer(file);
    }
  };
  const handleFileUploadBtnClick = () => {
    setModelPopupTrigger(true);
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
  /* Function to get task data */
  useEffect(() => {
    const fetchOne = async () => {
      const data = await CategoryService.getOneCategoryByID(
        Number(selectedCategory?.category_id)
      );
      setCategoryData(data?.data);
    };
    fetchOne();
    refetch();
    if (getAllData && getAllData.length) {
      let totalAmount = 0;
      getAllData.forEach((element: any) => {
        totalAmount = totalAmount + element.actual_budget;
      });
      setTotalAmount(totalAmount);
    }
  }, [selectedCategory?.category_id, getAllData, reload]);
  const handleClosePopup = (): void => {
    setTaskPopupTrigger(false);
  };
  const handleCloseModelPopup = () => {
    setModelPopupTrigger(false);
    setUploadedTaskData(null);
  };
  /* Function for bulk upload of task data */
  const handleTaskBulkUpload = () => {
    if (uploadedTaskData?.length) {
      const Object = [...uploadedTaskData];
      createNewMultipleSubcategory(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.status === true) {
            setModelPopupTrigger(false);
            setMessage('Task created');
            setOpenSnack(true);
            setUploadedTaskData(null);
            setReload(!reload);
          } else {
            setMessage('Error Ocurred');
            setOpenSnack(true);
            setIswarning(true);
          }
        },
      });
      setTaskPopupTrigger(false);
    } else {
      console.log('throw error');
    }
  };
  const staticData = [
    {
      s_no: '1',
      description: 'Sample Data - Details ',
      uom: 'Unit',
      quantity: '1',
      rate: '100',
      amount: '100',
    },
    {
      s_no: '1.1',
      description: 'Sample Data - Details ',
      uom: 'Unit',
      quantity: '1',
      rate: '100',
      amount: '100',
    },
    {
      s_no: '1.2',
      description: 'Sample Data - Details ',
      uom: 'Unit',
      quantity: '1',
      rate: '100',
      amount: '100',
    },
    {
      s_no: '2',
      description: 'Sample Data - Details ',
      uom: 'Unit',
      quantity: '1',
      rate: '100',
      amount: '100',
    },
  ];
  /* Function for converting json data into excel format */
  const convertToCSV = (data: any[]) => {
    const header = [
      'SI.NO',
      'Description',
      'Unit',
      'Quantity',
      'Rate',
      'Amount',
    ];
    const csvRows = [header.join(',')];
    for (const item of staticData) {
      const rowData = [
        item.s_no,
        item.description,
        item.uom,
        item.quantity,
        item.rate,
        item.amount,
      ];
      csvRows.push(rowData.join(','));
    }
    return csvRows.join('\n');
  };
  /* Function for downloading sample data */
  const handleExcelTemplateDownload = () => {
    const csvContent = convertToCSV(staticData);
    const blob = new Blob([csvContent], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'TaskData.csv';
    link.click();
    URL.revokeObjectURL(url);
  };

  return (
    <div className={Styles.task_page_container}>
      <div>
        <CustomPopupModel
          open={modelPopupTrigger}
          title={'Upload Task' || categoryData?.name}
          handleClose={handleCloseModelPopup}
          content={
            <div>
              <div
                className={`${Styles.flex} ${Styles.space_between}`}
                style={{
                  display: 'flex',
                  justifyContent: 'space-between',
                  alignItems: 'center',
                  padding: '2rem',
                }}
              >
                <div>
                  {uploadedTaskData?.length ? (
                    <Button
                      color="primary"
                      shape="rectangle"
                      size="small"
                      onClick={() => {
                        //bulkUpload task handler
                        handleTaskBulkUpload();
                      }}
                      justify="center"
                    >
                      Upload Task
                    </Button>
                  ) : (
                    <div>
                      <Button
                        color="primary"
                        shape="rectangle"
                        size="small"
                        icon={
                          <FileUploadIcon
                            width={20}
                            color="white"
                            onClick={function (): void {
                              console.log('');
                            }}
                          />
                        }
                        onClick={() => {
                          if (fileInputRef.current) {
                            fileInputRef.current.click();
                          }
                        }}
                      >
                        Upload File
                      </Button>
                      <span>
                        <input
                          ref={fileInputRef}
                          id="upload-photo"
                          name="upload_photo"
                          type="file"
                          style={{ display: 'none' }}
                          onChange={(e) => {
                            handleFileOnChange(e);
                          }}
                        />
                      </span>
                    </div>
                  )}
                </div>
                <div
                  style={{
                    display: 'flex',
                    justifyContent: 'space-between',
                    alignItems: 'center',
                    gap: '1rem',
                  }}
                >
                  <div>
                    {uploadedTaskData?.length ? (
                      <Button
                        color="primary"
                        shape="rectangle"
                        size="small"
                        onClick={() => {
                          setUploadedTaskData(null);
                          if (fileInputRef.current !== null) {
                            fileInputRef.current.value = '';
                          }
                        }}
                        justify="center"
                      >
                        Cancel
                      </Button>
                    ) : (
                      ''
                    )}
                  </div>

                  <Button
                    color="primary"
                    shape="rectangle"
                    size="small"
                    icon={
                      <DownloadIcon
                        width={20}
                        color="white"
                      />
                    }
                    onClick={() => {
                      handleExcelTemplateDownload();
                    }}
                  >
                    Download Template
                  </Button>
                </div>
              </div>
              {uploadedTaskData ? (
                <div
                  className={`${Styles.ab_tableContainer} ${Styles.boqSubCategoryTable}`}
                >
                  <table style={{ padding: '20px', width: '100%' }}>
                    <thead>
                      <tr style={{ padding: '20px 0' }}>
                        <th style={{ padding: '10px 10px' }}>S No</th>
                        <th style={{ padding: '10px 10px' }}>Description</th>
                        <th style={{ padding: '10px 10px' }}>Uom</th>
                        <th style={{ padding: '10px 10px' }}>Quantity</th>
                        <th style={{ padding: '10px 10px' }}>Rate</th>
                        <th style={{ padding: '10px 10px' }}>Amount</th>
                        <th style={{ padding: '10px 10px' }}>Action</th>
                      </tr>
                    </thead>
                    <tbody>
                      {uploadedTaskData && uploadedTaskData?.length > 0 ? (
                        uploadedTaskData.map((item: any, index: any) => (
                          <>
                            <tr key={index}>
                              <td style={{ padding: '10px 10px' }}>
                                {index + 1}
                              </td>
                              <td style={{ padding: '10px 10px' }}>
                                {item.description || '--'}
                              </td>
                              <td style={{ padding: '10px 10px' }}>
                                {item.uom_label || '--'}
                              </td>
                              <td style={{ padding: '10px 10px' }}>
                                {item.quantity || '--'}
                              </td>
                              <td style={{ padding: '10px 10px' }}>
                                {item.rate || '--'}
                              </td>
                              <td style={{ padding: '10px 10px' }}>
                                {item.estimated_budget || '--'}
                              </td>
                              <td
                                style={{
                                  padding: '10px 10px',
                                  cursor: 'pointer',
                                }}
                              >
                                <DeleteIcon
                                  onClick={() => {
                                    if (uploadedTaskData.length) {
                                      const data = [...uploadedTaskData];
                                      data.splice(index, 1);
                                      setUploadedTaskData(data);
                                    } else {
                                      console.log('throw error');
                                    }
                                  }}
                                />
                              </td>
                            </tr>
                            {item?.children.length
                              ? item.children.map(
                                  (child: any, childIndex: any) => (
                                    <tr key={childIndex}>
                                      <td style={{ padding: '10px 10px' }}>
                                        {index + 1 + '.' + (childIndex + 1)}
                                      </td>
                                      <td style={{ padding: '10px 10px' }}>
                                        {child.description || '--'}
                                      </td>
                                      <td style={{ padding: '10px 10px' }}>
                                        {child.uom_label || '--'}
                                      </td>
                                      <td style={{ padding: '10px 10px' }}>
                                        {child.quantity || '--'}
                                      </td>
                                      <td style={{ padding: '10px 10px' }}>
                                        {child.rate || '--'}
                                      </td>
                                      <td style={{ padding: '10px 10px' }}>
                                        {child.estimated_budget || '--'}
                                      </td>
                                    </tr>
                                  )
                                )
                              : ''}
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
                </div>
              ) : (
                <h1 className={Styles.file_upload_empty_label_container}>
                  {' '}
                  Upload File
                </h1>
              )}
            </div>
          }
          width={'90%'}
          description={'Import csv file to upload bulk data'}
        />
      </div>
      {isloading ? (
        <CustomLoader loading={isloading} size={48} />
      ) : (
        <div>
          {getAllData ? (
            <div>
              <div className={Styles.mainHeading}>
                <div className={Styles.mainLeftContent}>
                  <div className={Styles.leftContentOne}>
                    <CheckListIcon style={{ padding: '4px 0 0 0;' }} />
                    <h3
                      title={categoryData?.description}
                      style={{ width: '30rem' }}
                    >
                      {/* {categoryData?.name
                      ? categoryData?.name?.length > 20
                        ? categoryData?.name?.substring(0, 20) + '...'
                        : categoryData?.name
                      : '-'} */}
                      {categoryData?.description}({getAllData?.length})
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
                  <div
                    className={`${Styles.flex} ${Styles.gap_2} ${Styles.bulkUpload_container}`}
                    onClick={handleFileUploadBtnClick}
                    style={{ cursor: 'pointer' }}
                  >
                    <span>
                      <FileUploadIcon
                        width={20}
                        height={20}
                        color="#7f56d9"
                        onClick={function (): void {
                          throw new Error('Function not implemented.');
                        }}
                      />
                    </span>
                    <span className={Styles.menuFont}>Bulk Upload</span>
                  </div>
                </div>
                <div
                  style={{
                    display: 'flex',
                    flexDirection: 'row',
                    justifyContent: 'space-between',
                    gap: '2rem',
                  }}
                >
                  <div>
                    <h3>
                      {formatBudgetValue(
                        categoryData?.estimated_budget
                          ? categoryData?.estimated_budget
                          : 0
                      )}
                    </h3>
                    <p className={Styles.countContentTitle}>Estimated Budget</p>
                  </div>
                  <div>
                    <h3
                      style={{
                        color:
                          totalAmount > categoryData?.estimated_budget
                            ? 'red'
                            : '',
                      }}
                    >
                      {formatBudgetValue(totalAmount ? totalAmount : 0)}
                    </h3>
                    <p
                      className={Styles.countContentTitle}
                      style={{
                        color:
                          totalAmount > categoryData?.estimated_budget
                            ? 'red'
                            : '',
                      }}
                    >
                      Actual Budget
                    </p>
                  </div>
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
                      <th className={Styles.tableHeading}>Estimated Amount</th>
                      <th className={Styles.tableHeading}>Actual Amount</th>
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
                              selectedSubCategoryId === data?.sub_category_id
                                ? Styles.selectedRow
                                : ''
                            }
                          >
                            <td>{index + 1}</td>
                            <td
                              onClick={(e) => {
                                handleSubTaskView(data.sub_category_id);
                              }}
                              className={Styles.td_desc}
                            >
                              <span
                                style={{
                                  textAlign: 'justify',
                                  cursor: data?.children.length
                                    ? 'pointer'
                                    : '',
                                }}
                              >
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
                                {data?.estimated_budget
                                  ? formatBudgetValue(
                                      data?.estimated_budget
                                        ? data?.estimated_budget
                                        : 0
                                    )
                                  : '--'}
                              </span>
                            </td>
                            <td>
                              <span style={{ textAlign: 'justify' }}>
                                {data?.actual_budget
                                  ? formatBudgetValue(
                                      data?.actual_budget
                                        ? data?.actual_budget
                                        : 0
                                    )
                                  : '--'}
                              </span>
                            </td>
                            <td>
                              <div className={Styles.actionIcons_container}>
                                <span style={{ cursor: 'pointer' }}>
                                  <EditIcon
                                    onClick={() => {
                                      handleEdit(data?.sub_category_id);
                                      setSelectedSubCategoryId(
                                        data?.sub_category_id
                                      );
                                    }}
                                  />
                                </span>

                                <span>
                                  {!data.actual_budget ? (
                                    <span
                                      onClick={() => {
                                        handleSubTask(data?.sub_category_id);
                                        setSelectedSubCategoryId(
                                          data?.sub_category_id
                                        );
                                      }}
                                    >
                                      <AddIcon
                                        width={20}
                                        height={20}
                                        color={primary_color}
                                        style={{ cursor: 'pointer' }}
                                      />
                                    </span>
                                  ) : (
                                    ''
                                  )}
                                </span>

                                {data?.children?.length === 0 ? (
                                  <span
                                    onClick={() => {
                                      setSelectedSubCategoryId(
                                        data.sub_category_id
                                      );
                                      setPlanListTitle(data.name);
                                      setShowPlanForm(true);
                                      setSelectedSubCategory(data);
                                    }}
                                  >
                                    <SettingIcon
                                      style={{ cursor: 'pointer' }}
                                      color={primary_color}
                                    />
                                  </span>
                                ) : (
                                  <span
                                    onClick={(e) => {
                                      handleSubTaskView(data.sub_category_id);
                                    }}
                                    style={{
                                      textAlign: 'justify',
                                      cursor: data?.children.length
                                        ? 'pointer'
                                        : '',
                                      transform: data.switch
                                        ? 'rotate(180deg);'
                                        : '',
                                    }}
                                  >
                                    {subTaskView === false &&
                                    selectedSubCategoryId ===
                                      data.sub_category_id ? (
                                      <ExpandClose></ExpandClose>
                                    ) : (
                                      <ExpandIcon
                                        color={primary_color}
                                        style={{
                                          fill_opacity: data?.children.length
                                            ? ''
                                            : '.5',
                                        }}
                                      ></ExpandIcon>
                                    )}
                                  </span>
                                )}
                                {/* <span>  <CustomMenu actions={actions} name="BoQItems" /></span> */}
                              </div>
                            </td>
                          </tr>
                          {!subTaskView &&
                            selectedSubCategoryId === data?.sub_category_id &&
                            subChildList?.map((item: any, subindex: any) => {
                              return (
                                <SubBoqItems
                                  key={subindex}
                                  index={subindex}
                                  primaryIndex={index + 1}
                                  rowData={item}
                                  reload={reload}
                                  setReload={setReload}
                                  subTaskView={subTaskView}
                                  setSubTaskView={setSubTaskView}
                                  actions={undefined}
                                />
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
                title={'Manage Plans'}
                width="85%"
                handleClose={handleClosePlanList}
                content={
                  <PlanList
                    open={showPlanForm}
                    setOpen={setShowPlanForm}
                    subCategoryId={selectedSubCategoryId}
                    subCategory={selectedSubCategory}
                    reload={reload}
                    setReload={setReload}
                    setAbstractReload={setReload}
                    abstractReload={reload}
                  />
                }
                description={''}
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
                  <div
                    style={{
                      display: 'flex',
                      gap: '2rem',
                      alignItems: 'center',
                    }}
                  >
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
                    <Button
                      color="primary"
                      shape="rectangle"
                      size="small"
                      icon={
                        <FileUploadIcon
                          width={20}
                          color="white"
                          onClick={function (): void {
                            console.log('');
                          }}
                        />
                      }
                      onClick={() => {
                        setModelPopupTrigger(true);
                      }}
                    >
                      Upload File
                    </Button>
                  </div>
                </div>
              </div>
            </div>
          )}
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
            selectedCategoryId={selectedCategory?.category_id}
            selectedSubCategory={selectedSubCategoryId}
            setAbstractReload={setReload}
            abstractReload={reload}
          />
        }
      />
    </div>
  );
};
export default BomItems;
