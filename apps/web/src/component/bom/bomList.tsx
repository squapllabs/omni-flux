import React, { useEffect, useState } from 'react';
import { useGetAllCategory } from '../../hooks/category-hooks';
import Styles from '../../styles/bom.module.scss';
import DropdownIcon from '../menu/icons/dropDownButton';
import MoreVerticalIcon from '../menu/icons/moreVerticalIcon';
import subCategoryService from '../../service/subCategory-service';
import subSubCategoryService from '../../service/subSubCategory-service';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import CustomLoader from '../ui/customLoader';
import CustomBomAddPopup from '../ui/CustomBomAddPopup';
import CustomAbstractAddPopup from '../ui/CustomAbstractPopup';
import CustomSubCategoryAddPopup from '../ui/CustomSubCategoryPopup';
import CustomSubSubCategoryAddPopup from '../ui/CustomSubSubCategoryPopup';
import BomService from '../../service/bom-service';
import { formatBudgetValue } from '../../helper/common-function';

const BomList = () => {
  const [selectedCategory, setSelectedCategory] = useState(80);
  const [selectedSubCategory, setSelectedSubCategory] = useState();
  const [selectedSubSubCategory, setSelectedSubSubCategory] = useState();
  const [selectedSubSubChild, setSelectedSubSubChild] = useState();
  const [subCatList, setSubCatList] = useState([]);
  const [subSubCatList, setSubSubCatList] = useState([]);
  const [subSubChildList, setSubSubChildList] = useState([]);
  const [showItemForm, setShowItemForm] = useState(false);
  const [showAbstractForm, setShowAbstractForm] = useState(false);
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [showSubSubCategoryForm, setShowSubSubCategoryForm] = useState(false);
  const [open, setOpen] = useState(false);
  const [openSub, setOpenSub] = useState(false);
  const [openChild, setOpenChild] = useState(false);
  const [categoryData, setCategoryData] = useState();
  const [subCategoryData, setSubCategoryData] = useState();
  const [moreIconDropdownOpen, setMoreIconDropdownOpen] = useState(false);
  const [openedContextMenuForCategory, setOpenedContextMenuForCategory] =
    useState<number | null>(null);
  const [categoryId, setCategoryId] = useState();
  const [subCategoryIDd,setSubCategoryId] = useState();
  console.log('id to props--->', openedContextMenuForCategory);
// console.log("ddddddddddddd",subSubChildList?.description);

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
    console.log('initial obj', Obj);
    const bomData = await BomService.getCustomBomData(Obj);
    console.log('bomData ===>', bomData);
  };
  const handleSelectedCategory = async (value: any) => {
    console.log('category id selected ===>', value.category_id);
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
  const handleSelectedSubCategory = async (value: any) => {
    setSelectedSubCategory(value.sub_category_id);
    const subsubCatList = await subSubCategoryService.getOneSubSubCatListbySubCatID(value.sub_category_id);
    console.log("value===>",value);
    setSubCategoryData(value)
    setSubSubCatList(subsubCatList.data);
    if (subsubCatList?.data === null) {
      setOpenSub(false);
    } else {
      setOpenSub(!openSub);
    }
  };
  const handleSelectedSubSubCategory = async (value: any) => {
    console.log(value);
    setSelectedSubSubCategory(value);
    const childData = await subSubCategoryService.getChildbyParentID(value);
    setSubSubChildList(childData?.data);
    if (childData?.data.length === 0) {
      setOpenChild(false);
    } else {
      setOpenChild(!openChild);
    }
  };
  const handleSelectedSubSubChild = async (value: any) => {
    console.log(value);
    setSelectedSubSubChild(value);
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
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  fullWidth
                  icon={<AddIcon width={20} />}
                  onClick={() => {
                    setShowAbstractForm(true);
                  }}
                >
                  Abstract
                </Button>
                {categories?.map((items: any, index: any) => {
                  return (
                    <ul key={index}>
                      <li>
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
                            <MoreVerticalIcon
                              onClick={(e: any) => {
                                e.stopPropagation();
                                console.log('icon pressed');
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
                        {open && selectedCategory === items.category_id ? (
                          <div className={Styles.additional_content}>
                            {subCatList?.map((item: any, index: any) => {
                              return (
                                <ul key={index}>
                                  <li>
                                    <div
                                      className={
                                        selectedSubCategory ===
                                        item.sub_category_id
                                          ? Styles.selected
                                          : Styles.primarylistContent
                                      }
                                      onClick={() =>
                                        handleSelectedSubCategory(item)
                                      }
                                    >
                                      {item?.name}
                                      {/* <DropdownIcon /> */}
                                    </div>
                                    {openSub &&
                                    selectedSubCategory ===
                                      item.sub_category_id ? (
                                      <div
                                        className={Styles.additional_content}
                                      >
                                        {subSubCatList?.map(
                                          (item: any, index: any) => {
                                            return (
                                              <ul key={index}>
                                                <li>
                                                  <div
                                                    className={
                                                      selectedSubSubCategory ===
                                                      item.sub_sub_category_id
                                                        ? Styles.selected
                                                        : Styles.primarylistContent
                                                    }
                                                    onClick={() =>
                                                      handleSelectedSubSubCategory(
                                                        item.sub_sub_category_id
                                                      )
                                                    }
                                                  >
                                                    {item?.name}
                                                  </div>

                                                  {openChild &&
                                                  selectedSubSubCategory ===
                                                    item.sub_sub_category_id ? (
                                                    <div
                                                      className={
                                                        Styles.additional_content
                                                      }
                                                    >
                                                      {subSubChildList?.map(
                                                        (
                                                          item: any,
                                                          index: any
                                                        ) => {
                                                          return (
                                                            <ul key={index}>
                                                              <li>
                                                                <div
                                                                  className={
                                                                    selectedSubSubChild ===
                                                                    item.sub_sub_category_id
                                                                      ? Styles.selected
                                                                      : Styles.primarylistContent
                                                                  }
                                                                  onClick={() =>
                                                                    handleSelectedSubSubChild(
                                                                      item.sub_sub_category_id
                                                                    )
                                                                  }
                                                                >
                                                                  {item?.name}
                                                                  <DropdownIcon />
                                                                </div>
                                                              </li>
                                                            </ul>
                                                          );
                                                        }
                                                      )}
                                                    </div>
                                                  ) : (
                                                    ''
                                                  )}
                                                </li>
                                              </ul>
                                            );
                                          }
                                        )}
                                      </div>
                                    ) : (
                                      ''
                                    )}
                                  </li>
                                </ul>
                              );
                            })}
                          </div>
                        ) : (
                          ''
                        )}
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
                  <span className={Styles.descriptionContent}>
                    {categoryData?.description}
                  </span>
                </div>
                <div>
                  <p>
                    {formatBudgetValue(
                      categoryData?.budget ? categoryData?.budget : 0
                    )}
                  </p>
                </div>
                <div></div>
              </div>
                )}
              {subCategoryData && (
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
                </div>
                <div>
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
                </div>
                </div>
              )}
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
      <CustomSubSubCategoryAddPopup
        isVissible={showSubSubCategoryForm}
        onAction={setShowSubSubCategoryForm}
        selectedCategoryId={categoryId}
      />
    </div>
  );
};

export default BomList;
