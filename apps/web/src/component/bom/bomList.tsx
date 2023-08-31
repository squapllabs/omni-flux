import React, { useEffect, useState } from 'react';
import { useGetAllCategory } from '../../hooks/category-hooks';
import Styles from '../../styles/bom.module.scss';
import DropdownIcon from '../menu/icons/dropDownButton';
import subCategoryService from '../../service/subCategory-service';
import subSubCategoryService from '../../service/subSubCategory-service';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import CustomLoader from '../ui/customLoader';
import CustomBomAddPopup from '../ui/CustomBomAddPopup';
import BomService from '../../service/bom-service';
const BomList = () => {
  const [selectedCategory, setSelectedCategory] = useState();
  const [selectedSubCategory, setSelectedSubCategory] = useState();
  const [selectedSubSubCategory, setSelectedSubSubCategory] = useState();
  const [selectedSubSubChild, setSelectedSubSubChild] = useState();
  const [subCatList, setSubCatList] = useState([]);
  const [subSubCatList, setSubSubCatList] = useState([]);
  const [subSubChildList, setSubSubChildList] = useState([]);
  const [showClientForm, setShowClientForm] = useState(false);
  const [open, setOpen] = useState(false);
  const [openSub, setOpenSub] = useState(false);
  const [openChild, setOpenChild] = useState(false);
  const { data: categories, isLoading: categoriesLoader } = useGetAllCategory();
  useEffect(() => {
    handleLoadData();
  });
  const handleLoadData = async () => {
    const Obj: any = {
      category_id: selectedCategory,
      sub_category_id: null,
      sub_sub_category_id: null,
    };
    const bomData = await BomService.getCustomBomData(Obj);
    console.log('bomData', bomData);
  };
  const handleSelectedCategory = async (value: any) => {
    console.log(value);
    setSelectedCategory(value);
    const subCatList = await subCategoryService.getOneSubCatListbyCatID(value);
    setSubCatList(subCatList.data);
    if (subCatList?.data === null) {
      setOpen(false);
    } else {
      setOpen(!open);
    }
  };
  const handleSelectedSubCategory = async (value: any) => {
    setSelectedSubCategory(value);
    const subsubCatList =
      await subSubCategoryService.getOneSubSubCatListbySubCatID(value);
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
  return (
    <div>
      <CustomLoader loading={categoriesLoader}>
        <div className={Styles.container}>
          <div className={Styles.subHeader}>
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon width={20} />}
              >
                Abstract
              </Button>
            </div>
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon width={20} />}
                onClick={() => {
                  setShowClientForm(true);
                }}
              >
                Item
              </Button>
            </div>
          </div>
          <div className={Styles.subcontainer}>
            <div className={Styles.submenu}>
              <div className={Styles.side_menu}>
                {categories?.map((items: any, index: any) => {
                  return (
                    <ul key={index}>
                      <li>
                        <div
                          className={
                            selectedCategory === items.category_id
                              ? Styles.selected
                              : Styles.primarylistContent
                          }
                          onClick={() =>
                            handleSelectedCategory(items.category_id)
                          }
                        >
                          {items?.name}
                          <DropdownIcon />
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
                                        handleSelectedSubCategory(
                                          item.sub_category_id
                                        )
                                      }
                                    >
                                      {item?.name}
                                      <DropdownIcon />
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
                                                    <DropdownIcon />
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
              <div>aaa</div>
              <div>aaa</div>
              <div>aaa</div>
              <div>aaa</div>
              <div>aaa</div>
            </div>
          </div>
        </div>
      </CustomLoader>
      <CustomBomAddPopup
        isVissible={showClientForm}
        onAction={setShowClientForm}
      />
    </div>
  );
};

export default BomList;
