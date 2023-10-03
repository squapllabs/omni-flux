import React, { useEffect, useState } from 'react';
import {
  useGetAllCategory,
  useGetAllCategoryByProjectId,
  useDeleteCategory,
} from '../../hooks/category-hooks';
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
import EditIcon from '../menu/icons/editIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import CustomSnackBar from '../ui/customSnackBar';
import CustomDelete from '../ui/customDeleteDialogBox';
import { getByProjectId } from '../../hooks/project-hooks';
import BackArrow from '../menu/icons/backArrow';
import ZIcon from '../menu/icons/zIcon';
import CheckListIcon from '../menu/icons/checkListIcon';

const BomList = () => {
  const params = useParams();
  const navigate = useNavigate();
  const projectId = Number(params?.projectId);
  const bomconfigId = Number(params?.bomconfigId);
  // console.log('oooo', params);
  // const projectId = Number(params?.projectId);
  const { data: projectData } = getByProjectId(projectId);
  // console.log('projectData', projectData);

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

  useEffect(() => {
    const fetchData = async () => {
      const obj = {
        projectId: projectId,
        bomconfigId: bomconfigId,
      };
      const datas = await CategoryService.getAllCategoryByProjectId(obj);
      console.log("rrrrrrrrrrrrrrrr",datas.data);
      setCategories(datas.data);
      setIsloading(false);
      setCategoryData(datas.data[0]);
      console.log("ttttttttttt",datas.data[0]);
      
      setSelectedCategory(datas.data[0].category_id);
      setCategoryId(datas.data[0].category_id);
    };
    fetchData();
  }, [reload]);

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

  return (
    <div>
      {isloading ? (
        <CustomLoader loading={isloading} size={48} />
      ) : (
        <div className={Styles.container}>
          {categories ? (
            <div className={Styles.subHeader}>
              <div className={Styles.subcontainer}>
                <div className={Styles.submenu}>
                  <div className={Styles.side_menu}>
                    <div className={Styles.topSideMenu}>
                      <div className={Styles.topSideHeading}>
                        <ZIcon />
                        <h3>Abstracts ({categories?.length})</h3>
                      </div>
                      <NewAddCircleIcon
                        onClick={() => {
                          setShowAbstractForm(true);
                          setMode('Add');
                        }}
                      />
                    </div>

                    {categories?.map((items: any, index: any) => {
                      return (
                        <ul key={index}>
                          <li>
                            {/* it is category shows */}
                            <div
                              style={{
                                display: 'flex',
                                justifyContent: 'space-between',
                              }}
                            >
                              <div
                                className={
                                  selectedCategory === items.category_id
                                    ? Styles.selected
                                    : Styles.primarylistContent
                                }
                                onClick={() => {
                                  handleSelectedCategory(items);
                                  setCategoryId(items.category_id);
                                }}
                              >
                                <div>
                                  {index + 1} {items?.name}
                                  <span className={Styles.smallred}>
                                    {items?.progress_status}
                                  </span>
                                </div>
                              </div>
                              <div>
                                {/* category add  */}
                                <MoreVerticalIcon
                                  onClick={(e: any) => {
                                    e.stopPropagation();
                                    setOpenedContextMenuForCategory(
                                      items.category_id
                                    );
                                    setCategoryId(items.category_id);
                                    setMoreIconDropdownOpen(
                                      !moreIconDropdownOpen
                                    );
                                  }}
                                />
                                {moreIconDropdownOpen &&
                                  items.category_id ===
                                    openedContextMenuForCategory && (
                                    <ul className={Styles.menu}>
                                      <li className={Styles.menuItem}>
                                        <div
                                          style={{
                                            display: 'flex',
                                            flexDirection: 'column',
                                            gap: '5px',
                                            padding: '5px',
                                          }}
                                        >
                                          <div
                                            className={Styles.options}
                                            onClick={() =>
                                              handleEdit(items.category_id)
                                            }
                                          >
                                            <span className={Styles.menuFont}>
                                              Edit Abstract
                                            </span>
                                          </div>
                                          <div
                                            className={Styles.options}
                                            onClick={() =>
                                              deleteHandler(items.category_id)
                                            }
                                          >
                                            <span className={Styles.menuFont}>
                                              Delete
                                            </span>
                                          </div>
                                        </div>
                                      </li>
                                    </ul>
                                  )}
                              </div>
                            </div>
                          </li>
                        </ul>
                      );
                    })}
                  </div>
                </div>
                <div className={Styles.mainContainer}>
                  {categoryData && (
                    <div>
                      <div className={Styles.mainHeading}>
                        <div className={Styles.mainLeftContent}>
                          <div className={Styles.leftContentOne}>
                            <CheckListIcon/>
                            <h3>{categoryData?.name}(count)</h3>
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
                          <p>INR {formatBudgetValue(
                              categoryData?.budget ? categoryData?.budget : 0
                            )}</p>
                          <p>
                            Aggregated Value
                          </p>
                        </div>
                      </div>
                    </div>
                  )}
                  <BomItems
                    selectedCategory={selectedCategory}
                    setSelectedSubCategory={setSelectedSubCategory}
                    selectedSubCategory={selectedSubCategory}
                    projectsId={projectsId}
                    selectedBomConfig={bomconfigId}
                  />
                </div>
              </div>
            </div>
          ) : (
            <div className={Styles.Secondcontainer}>
              <div className={Styles.abstractButton}>
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
            </div>
          )}
        </div>
      )}
      <CustomBomAddPopup isVissible={showItemForm} onAction={setShowItemForm} />
      <CustomAbstractAddPopup
        isVissible={showAbstractForm}
        onAction={setShowAbstractForm}
        selectedProject={projectsId}
        selectedBomConfig={bomconfigId}
        setReload={setReload}
        mode={mode}
        setMode={setMode}
        categoryId={categoryId}
      />
      <CustomSubCategoryAddPopup
        isVissible={showSubCategoryForm}
        onAction={setShowSubCategoryForm}
        selectedCategoryId={categoryId}
        selectedProject={projectsId}
        selectedBomConfig={bomconfigId}
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
