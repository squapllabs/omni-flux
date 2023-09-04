import React, { useEffect, useState } from 'react';
import { useGetAllCategory } from '../../hooks/category-hooks';
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

const BomList = () => {
  const [selectedCategory, setSelectedCategory] = useState(80);
  const [showItemForm, setShowItemForm] = useState(false);
  const [showAbstractForm, setShowAbstractForm] = useState(false);
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [open, setOpen] = useState(false);
  const [categoryData, setCategoryData] = useState();
  console.log('rrrrrrrrrrr', categoryData);
  const [moreIconDropdownOpen, setMoreIconDropdownOpen] = useState(false);
  const [openedContextMenuForCategory, setOpenedContextMenuForCategory] =
    useState<number | null>(null);
  const [categoryId, setCategoryId] = useState();

  console.log('mainData ==>', categoryData);

  const { data: categories, isLoading: categoriesLoader } = useGetAllCategory();
  console.log('categories data===>', categories);

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
    setSubCatList(subCatList.data);
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
      <CustomLoader loading={categoriesLoader}>
        <div className={Styles.container}>
          <div className={Styles.subHeader}></div>
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
                            onClick={() => handleSelectedCategory(items)}
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
                                    onClick={() => setShowSubCategoryForm(true)}
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
                                      <span>Sub Category</span>
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
              {/* {subCategoryData && (
                <div className={Styles.mainHeading}>
                  <div className={Styles.mainLeftContent}>
                    <span className={Styles.descriptionContentOne}>
                      {subCategoryData?.description}
                    </span>
                  </div>
                  <div>
                    <p>
                      {formatBudgetValue(
                        subCategoryData?.budget ? subCategoryData?.budget : 0
                      )}
                    </p>
                  </div> */}
              {/* <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      icon={<AddIcon width={20} />}
                      onClick={() => {
                        setShowItemForm(true);
                      }}
                    >
                      Item
                    </Button>
                  </div> */}
              {/* </div>
              )} */}
            </div>
          </div>
        </div>
      </CustomLoader>
      <CustomBomAddPopup isVissible={showItemForm} onAction={setShowItemForm} />
      <CustomAbstractAddPopup
        isVissible={showAbstractForm}
        onAction={setShowAbstractForm}
      />
      <CustomSubCategoryAddPopup
        isVissible={showSubCategoryForm}
        onAction={setShowSubCategoryForm}
        selectedCategoryId={categoryId}
      />
    </div>
  );
};

export default BomList;
