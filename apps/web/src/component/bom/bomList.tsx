import React, { useEffect, useState } from 'react';
import {
  useGetAllCategory,
  useGetAllCategoryByProjectId,
} from '../../hooks/category-hooks';
import Styles from '../../styles/bom.module.scss';
import MoreVerticalIcon from '../menu/icons/moreVerticalIcon';
import subCategoryService from '../../service/subCategory-service';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import CustomLoader from '../ui/customLoader';
import CustomBomAddPopup from '../ui/CustomBomAddPopup';
import CustomAbstractAddPopup from '../ui/CustomAbstractPopup';
import CustomSubCategoryAddPopup from '../ui/CustomSubCategoryPopup';
import BomService from '../../service/bom-service';
import { formatBudgetValue } from '../../helper/common-function';
import BomItems from './bomItems';
import Bom from './bom';
import { useParams } from 'react-router-dom';
import CategoryService from '../../service/category-service';

const BomList = () => {
  const params = useParams();
  const projectId = Number(params?.projectId);
  console.log('pppppppp', projectId);
  const [projectsId, setProjectsId] = useState(projectId);
  const [selectedCategory, setSelectedCategory] = useState();
  const [selectedSubCategory, setSelectedSubCategory] = useState();
  const [showItemForm, setShowItemForm] = useState(false);
  const [showAbstractForm, setShowAbstractForm] = useState(false);
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [open, setOpen] = useState(false);
  const [categoryData, setCategoryData] = useState();
  console.log('selectedSubCategory', selectedSubCategory);
  const [moreIconDropdownOpen, setMoreIconDropdownOpen] = useState(false);
  const [openedContextMenuForCategory, setOpenedContextMenuForCategory] =
    useState<number | null>(null);
  const [categoryId, setCategoryId] = useState();
  const [categories,setCategories] = useState();
  const [reload,setReload] = useState(false);
  // const { data: categories, isLoading: categoriesLoader } = useGetAllCategoryByProjectId(projectId);
  // console.log('categories data===>', categories);
  // console.log('mainData ==>', categoryData);

  useEffect(() => {
    const fetchData = async () => {
      const datas = await CategoryService.getAllCategoryByProjectId(projectId);
      console.log('use effect api called ===>', datas);
      setCategories(datas.data);
    };
    fetchData();
  }, [reload]);

  useEffect(() => {
    handleLoadData();
  }, [selectedCategory]);
  const handleLoadData = async () => {
    const Obj: any = {
      category_id: selectedCategory,
      sub_category_id: null,
      sub_sub_category_id: null,
    };
    const bomData = await BomService.getCustomBomData(Obj);
  };
  const handleSelectedCategory = async (value: any) => {
    setSelectedCategory(value.category_id);
    setCategoryData(value);
    const subCatList = await subCategoryService.getOneSubCatListbyCatID(
      value.category_id
    );
    console.log('line 62 --->', subCatList);
    // setSubCatList(subCatList.data);
    if (subCatList?.data === null) {
      setOpen(false);
    } else {
      setOpen(!open);
    }
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
      {/* <CustomLoader loading={categoriesLoader}> */}
      <div className={Styles.container}>
        <div className={Styles.subHeader}>
          <div className={Styles.subcontainer}>
            <div className={Styles.submenu}>
              <div className={Styles.side_menu}>
                <div className={Styles.topSideMenu}>
                  <div className={Styles.topSideHeading}>
                    <h3>BOQ Creator</h3>
                  </div>
                  <Button
                    color="secondary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon width={20} />}
                    onClick={() => {
                      setShowAbstractForm(true);
                    }}
                  >
                    Add Abstract
                  </Button>
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
                            {items?.name}
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
                                setMoreIconDropdownOpen(!moreIconDropdownOpen);
                              }}
                            />
                            {moreIconDropdownOpen &&
                              items.category_id ===
                                openedContextMenuForCategory && (
                                <ul className={Styles.menu}>
                                  <li
                                    className={Styles.menuItem}
                                    // onClick={() => setShowSubCategoryForm(true)}
                                  >
                                    <div
                                      style={{
                                        display: 'flex',
                                        alignItems: 'center',
                                        gap: '5px',
                                      }}
                                    >
                                      <div>
                                        <AddIcon width={20} />
                                      </div>
                                      <span>Options</span>
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
                <div className={Styles.mainHeading}>
                  <div className={Styles.mainLeftContent}>
                    <h3>{categoryData?.name}</h3>
                    <p className={Styles.descriptionContent}>
                      {categoryData?.description}
                    </p>
                  </div>
                  <div>
                    <p>Allocated Budget</p>
                    <p>
                      {formatBudgetValue(
                        categoryData?.budget ? categoryData?.budget : 0
                      )}
                    </p>
                  </div>
                </div>
              )}
              {selectedSubCategory === undefined ? (
                <>
                  <div className={Styles.taskButton}>
                    <Button
                      color="secondary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      icon={<AddIcon width={20} />}
                      onClick={() => {
                        setShowSubCategoryForm(true);
                      }}
                    >
                      Add Tasks
                    </Button>
                  </div>
                  <BomItems
                    selectedCategory={selectedCategory}
                    setSelectedSubCategory={setSelectedSubCategory}
                    selectedSubCategory={selectedSubCategory}
                  />
                </>
              ) : (
                <Bom selectedSubCategory={selectedSubCategory} />
              )}
            </div>
          </div>
        </div>
      </div>
      {/* </CustomLoader> */}
      <CustomBomAddPopup isVissible={showItemForm} onAction={setShowItemForm} />
      <CustomAbstractAddPopup
        isVissible={showAbstractForm}
        onAction={setShowAbstractForm}
        selectedProject={projectsId}
        setReload={setReload}
      />
      <CustomSubCategoryAddPopup
        isVissible={showSubCategoryForm}
        onAction={setShowSubCategoryForm}
        selectedCategoryId={categoryId}
        selectedProject={projectsId}
      />
    </div>
  );
};

export default BomList;
