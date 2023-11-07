import React, { useEffect, useRef, useState } from 'react';
import { formatBudgetValue } from '../../helper/common-function';
import subCategoryService from '../../service/subCategory-service';
import CustomMenu from '../ui/NewCustomMenu';
import Styles from '../../styles/newStyles/bomlist.module.scss';
import MoreVerticalIcon from '../menu/icons/moreVerticalIcon';
import ProjectTaskAdd from './forms/ProjectTaskAdd';
import CustomSidePopup from '../ui/CustomSidePopup';
import PlanList from './planList';
import EditIcon from '../menu/icons/newEditIcon';
import AddIcon from '../menu/icons/addIcon'
import ExpandIcon from '../menu/icons/expandIcon';
import ExpandClose from '../menu/icons/expandClose';
import SettingIcon from '../menu/icons/settingIcon';

interface SubBoqItemsProps {
  rowData: any;
  index: any;
  actions: any;
  primaryIndex: any;
  reload: any;
  setReload: any;
  subTaskView: any;
  setSubTaskView: any;
}

const SubBoqItems: React.FC<SubBoqItemsProps> = ({
  rowData,
  index,
  actions,
  primaryIndex,
  reload,
  setReload,
  subTaskView,
  setSubTaskView,
}) => {
  const [isCollapsed, setIsCollapsed] = useState(true);
  const [subChildData, setSubChildData] = useState<any>([]);
  const [selectedSubCategoryId, setSelectedSubCategoryId] = useState<any>();
  const [selectedCategory, setSelectedCategory] = useState();
  const [selectedBomConfig, setSelectedBomConfig] = useState();
  const [projectId, setProjectId] = useState();
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [showPlanForm, setShowPlanForm] = useState(false);
  const [planListTitle, setPlanListTitle] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [mode, setMode] = useState('');
  // const [reload, setReload] = useState(false);
  const [isOpen, setIsOpen] = useState(false);

  const toggleCollapse = async (value: any) => {

    setIsCollapsed(!isCollapsed);
    if(isCollapsed){
      setSelectedSubCategoryId(value);
    } else {
      setSelectedSubCategoryId('')
    }
    
    const getSubChildList =
      await subCategoryService.getOneChlidSubCatListbyParentID(value);
    console.log('getSubChildList', getSubChildList);
    setSubChildData(getSubChildList?.data);
  };
  const menuRef = useRef(null);
  const toggleMenu = () => {
    setIsOpen(!isOpen);
  };
  const primary_color = '#7f56d'
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

  const handleCloseTask = () => {
    setShowSubCategoryForm(false);
  };
  const handleClosePlanList = () => {
    setShowPlanForm(false);
  };
  const handleEditTask = (value: any) => {
    console.log('handleEditTask', value);
    setMode('EDIT');
    setShowSubCategoryForm(true);
    setIsOpen(false);
    setSelectedSubCategoryId(value.sub_category_id);
    setSelectedCategory(value.category_id);
  };
  const handleMangePlan = (value: any) => {
    setIsOpen(false);
    setShowPlanForm(true);
    setPlanListTitle(value.name);
    setSelectedSubCategoryId(value.sub_category_id);
  };
  const handleSubTask = (value: any) => {
    console.log('handleSubTask', value);
    setMode('Sub Task');
    setShowSubCategoryForm(true);
    setIsOpen(false);
    setSelectedSubCategoryId(value.sub_category_id);
    setProjectId(value.project_id);
    setSelectedBomConfig(value.bom_configuration_id);
    setSelectedCategory(value.category_id);
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
        <td >
          {primaryIndex + '.' + `${(index + 1)}`}
        </td>
        <td >
          {rowData.description}
        </td>
        <td>
          <span style={{ textAlign: 'justify' }}>
            {rowData?.uom_data?.name || '--'}
          </span>
        </td>
        <td>
          <span style={{ textAlign: 'justify' }}>
            {rowData.quantity || '--'}
          </span>
        </td>
        <td>
          <span style={{ textAlign: 'justify' }}>
            {rowData.rate || '--'}
          </span>
        </td>
        <td >
          {
          formatBudgetValue(
            rowData?.estimated_budget ? rowData?.estimated_budget : 0
          )  + '  ' + '('+
          formatBudgetValue(
            rowData?.actual_budget ? rowData?.actual_budget : 0
          ) + ')'}
        </td>
        <td>
          {/* <CustomMenu
            actions={[
              {
                label: 'Manage Plans',
                onClick: () => {
                  handleMangePlan(rowData);
                },
                disabled: rowData?.children?.length > 0,
              },
              {
                label: 'Edit Task',
                onClick: () => {
                  handleEditTask(rowData);
                },
              },
              {
                label: 'Add Sub Task',
                onClick: () => {
                  handleSubTask(rowData);
                },
                disabled: rowData?.is_bom_detail === true,
              },
            ]}
            name="BoQItems"
          /> */}
          <div className={Styles.iconContainer}>

          <div className={Styles.actionIcons_container}>
            <span style={{cursor: 'pointer'}}><EditIcon
              onClick={() => handleEditTask(rowData)}
            /></span>

            <span>
              {!rowData.actual_budget ?(
                <span 
                onClick={()=>handleSubTask(rowData)}
                >
                  <AddIcon width={20} height={20} color={primary_color} />
                </span>
              ):('')}
            </span>
            
            {
                rowData?.children?.length===0 ? (
                  <span
              onClick={()=>{
                handleMangePlan(rowData)
              }}
              >
                <SettingIcon
                style={{cursor: 'pointer'}}
                color={primary_color}
                />
              </span>
                ): (
                  <span
              onClick={(e) =>{
                toggleCollapse(rowData.sub_category_id)
                }}
              style={{ textAlign: 'justify' ,cursor: rowData?.children.length ? 'pointer':''}}
              >
                {isCollapsed === false && selectedSubCategoryId === rowData.sub_category_id?
                (<ExpandClose></ExpandClose>)
                :(
                  <ExpandIcon
                  color={primary_color}
                  style={{fill_opacity : rowData?.children.length?'':'.5'}}
                  ></ExpandIcon>
                )
                }

               
              </span>
                )
            }          
          </div>
            {/* <div
              onClick={(e) => {
                toggleMenu();
              }}
              className={Styles.menuText}
            >
              <MoreVerticalIcon />
            </div> */}
          </div>
          {isOpen && (
            <div className={Styles.customMenu} ref={menuRef}>
              <div className={Styles.menuDropdownItems}>
                <div
                  onClick={() => handleMangePlan(rowData)}
                  style={{
                    display: rowData?.children?.length > 0 ? 'none' : '',
                  }}
                  className={Styles.menuItem}
                >
                  Manage Plan
                </div>
                <div
                  onClick={() => handleEditTask(rowData)}
                  className={Styles.menuItem}
                >
                  Edit Task
                </div>
                <div
                  onClick={() => handleSubTask(rowData)}
                  style={{
                    display: rowData?.is_bom_detail === true ? 'none' : '',
                  }}
                  className={Styles.menuItem}
                >
                  Add Sub Task
                </div>
              </div>
            </div>
          )}
        </td>
      </tr>
      {!isCollapsed &&
        selectedSubCategoryId === rowData?.sub_category_id &&
        subChildData?.map((items: any, index: any) => {
          return (
            <>
              <SubBoqItems
                key={index}
                index={index}
                rowData={items}
                primaryIndex={`${primaryIndex}` + '.' + (index +1) }
              />
            </>
          );
        })}

      <div>
        <CustomSidePopup
          open={showSubCategoryForm}
          title={mode === 'EDIT' ? 'Edit Sub Task' : 'Create New Sub Task'}
          handleClose={handleCloseTask}
          content={
            <ProjectTaskAdd
              open={showSubCategoryForm}
              setOpen={setShowSubCategoryForm}
              selectedProject={projectId}
              selectedBomConfig={selectedBomConfig}
              selectedCategoryId={selectedCategory}
              selectedSubCategory={selectedSubCategoryId}
              reload={reload}
              setReload={setReload}
              openSnack={openSnack}
              setOpenSnack={setOpenSnack}
              message={message}
              setMessage={setMessage}
              mode={mode}
              subTaskView={subTaskView}
              setSubTaskView={setSubTaskView}
              isCollapsed={isCollapsed}
              setIsCollapsed={setIsCollapsed}
            />
          }
        />
        <CustomSidePopup
          open={showPlanForm}
          title={planListTitle || 'Manage Plans'}
          width="85%"
          handleClose={handleClosePlanList}
          content={
            <PlanList
              open={showPlanForm}
              setOpen={setShowPlanForm}
              subCategoryId={selectedSubCategoryId}
              reload={reload}
              setReload={setReload}
              subTaskView={subTaskView}
              setSubTaskView={setSubTaskView}
              isCollapsed={isCollapsed}
              setIsCollapsed={setIsCollapsed}
            />
          }
        />
      </div>
    </>
  );
};

export default SubBoqItems;
