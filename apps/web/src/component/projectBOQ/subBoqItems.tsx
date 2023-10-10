import React, { useEffect, useRef, useState } from 'react';
import { formatBudgetValue } from '../../helper/common-function';
import subCategoryService from '../../service/subCategory-service';
import CustomMenu from '../ui/NewCustomMenu';
import Styles from '../../styles/newStyles/bomlist.module.scss';
import MoreVerticalIcon from '../menu/icons/moreVerticalIcon';
interface SubBoqItemsProps {
  rowData: any;
  index: any;
  actions: any;
  primaryIndex: any;
}

const SubBoqItems: React.FC<SubBoqItemsProps> = ({
  rowData,
  index,
  actions,
  primaryIndex,
}) => {
  const [isCollapsed, setIsCollapsed] = useState(true);
  const [subChildData, setSubChildData] = useState<any>([]);
  const [selectedSubCategoryId, setSelectedSubCategoryId] = useState();
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [mode, setMode] = useState('');
  const [isOpen, setIsOpen] = useState(false);
  const toggleCollapse = async (value: any) => {
    setIsCollapsed(!isCollapsed);
    setSelectedSubCategoryId(value);
    const getSubChildList =
      await subCategoryService.getOneChlidSubCatListbyParentID(value);
    console.log('getSubChildList', getSubChildList);
    setSubChildData(getSubChildList?.data);
  };
  const menuRef = useRef(null);
  const toggleMenu = () => {
    setIsOpen(!isOpen);
  };
  useEffect(() => {
    const handleClickOutside = (event: any) => {
      if (menuRef.current && !menuRef.current.contains(event.target)) {
        setIsOpen(false);
      }
    };
    document.addEventListener('mousedown', handleClickOutside);
    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
    };
  }, []);

  const handleEditTask = (value: any) => {
    setMode('EDIT');
    setShowSubCategoryForm(true);
    setSelectedSubCategoryId(value);
  };
  const handleMangePlan = (value: any) => {
    setMode('EDIT');
    setShowSubCategoryForm(true);
    setSelectedSubCategoryId(value);
  };
  const handleSubTask = (value: any) => {
    console.log('data?.sub_category_id', value);
    setMode('Sub Task');
    setShowSubCategoryForm(true);
    setSelectedSubCategoryId(value);
  };
  return (
    <>
      <tr
        className={
          selectedSubCategoryId === rowData?.sub_category_id
            ? Styles.selectedRow
            : ''
        }
      >
        <td onClick={(e) => toggleCollapse(rowData.sub_category_id)}>
          {primaryIndex + 1 + '.' + `${index + 1}`}
        </td>
        <td onClick={(e) => toggleCollapse(rowData.sub_category_id)}>
          {rowData.description}
        </td>
        <td onClick={(e) => toggleCollapse(rowData.sub_category_id)}>
          {formatBudgetValue(
            rowData?.actual_budget ? rowData?.actual_budget : 0
          )}
        </td>
        <td>
          <CustomMenu actions={actions} name="BoQItems" />
          {/* <div className={Styles.customMenu} ref={menuRef}>
            <span className={Styles.menuText} onClick={toggleMenu}>
              <MoreVerticalIcon />
            </span>
            {isOpen && (
              <div className={Styles.menuDropdownItems}>
                <div onClick={() => handleMangePlan(rowData.sub_category_id)}>
                  Manage Plan
                </div>
                <div onClick={() => handleEditTask(rowData.sub_category_id)}>
                  Edit Task
                </div>
                <div onClick={() => handleSubTask(rowData.sub_category_id)}>
                  Add Sub Task
                </div>
              </div>
            )}
          </div> */}
        </td>
      </tr>
      {!isCollapsed &&
        selectedSubCategoryId == rowData?.sub_category_id &&
        subChildData?.map((items: any, index: any) => {
          return (
            <>
              <SubBoqItems
                key={index}
                index={index}
                rowData={items}
                primaryIndex={`${primaryIndex + 1}` + '.' + index}
              />
            </>
          );
        })}
    </>
  );
};

export default SubBoqItems;
