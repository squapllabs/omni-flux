import React, { useEffect, useState } from 'react';
import {
  getBycategoryIdInSub,
  useDeleteSubcategory,
} from '../../hooks/subCategory-hooks';
import Styles from '../../styles/newStyles/bomlist.module.scss';
import AddIcon from '../menu/icons/addIcon';
import { formatBudgetValue } from '../../helper/common-function';
import { useNavigate } from 'react-router-dom';
import bomService from '../../service/bom-service';
import CustomGroupButton from '../ui/CustomGroupButton';
import EditIcon from '../menu/icons/editIcon';
import CustomSubCategoryAddPopup from '../ui/CustomSubCategoryPopup';
import DeleteIcon from '../menu/icons/deleteIcon';
import CustomSnackBar from '../ui/customSnackBar';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomLoader from '../ui/customLoader';
import MoreVerticalIcon from '../menu/icons/moreVerticalIcon';


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
  const { data: getAllData } = getBycategoryIdInSub(obj);
  console.log('getAllData----------90', getAllData);

  const { mutate: getDeleteSubCategoryByID } = useDeleteSubcategory();
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [selectedSubCategoryId, setSelectedSubCategoryId] = useState();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [isWarning, setIswarning] = useState(false);
  const [mode, setMode] = useState('');
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [value, setValue] = useState();
  const navigate = useNavigate();
  const [isExpanded, setIsExpanded] = useState(null);
  const [tableData, setTableData] = useState();
  const [istableLoader, setIsTableLoader] = useState(true);
  const [activeButton, setActiveButton] = useState<string | null>('RAWMT');
  const [moreIconDropdownOpen, setMoreIconDropdownOpen] = useState(false);
  const [openedContextMenuForSubCategory, setOpenedContextMenuForSubCategory] =
    useState<number | null>(null);
//   const [buttonLabels, setButtonLabels] = useState([
//     { label: 'RAW MATERIAL', value: 'RAWMT' },
//     { label: 'LABOUR', value: 'LABOR' },
//     { label: 'MACHINERY', value: 'MCNRY' },
//   ]);

  const handleEdit = (value: any) => {
    setMode('EDIT');
    setShowSubCategoryForm(true);
    setSelectedSubCategoryId(value);
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
//   const handleGroupButtonClick = async (value: string) => {
//     setIsTableLoader(true);
//     setActiveButton(value);
//     const obj = {
//       id: isExpanded,
//       type: value,
//     };
//     try {
//       const getData = await bomService.getBOMbySubCatIDandType(obj);
//       setTableData(getData.data);
//       setIsTableLoader(false);
//     } catch (error) {
//       console.error('Error fetching data in handleGroupButtonClick :', error);
//     }
//   };

//   const handleDemo = async (subCategoryId: any) => {
//     const obj = {
//       id: subCategoryId,
//       type: activeButton,
//     };
//     if (isExpanded === subCategoryId) {
//       setIsExpanded(null);
//       setActiveButton('RAWMT');
//     } else {
//       const getData = await bomService.getBOMbySubCatIDandType(obj);
//       setActiveButton('RAWMT');
//       setTableData(getData.data);
//       setIsExpanded(subCategoryId);
//       setIsTableLoader(false);
//     }
//   };

  useEffect(() => {
    // handleDemo()
  }, [activeButton]);

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

  return (
    <div>
      <div className={Styles.tableContainer}>
        <div>
          <table className={Styles.boqSubCategoryTable}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>#</th>
                <th className={Styles.tableHeading}>Task Name</th>
                <th className={Styles.tableHeading}>Task Description</th>
                <th className={Styles.tableHeading}>Amount</th>
                <th className={Styles.tableHeading}>Action</th>
              </tr>
            </thead>
            <tbody>
              {getAllData?.map((data: any, index: number) => (
                <tr key={data.sub_category_id}>
                  <td>{index + 1}</td>
                  <td><span title={data?.name}>
                      {data.name
                        ? data.name.length > 20
                          ? data.name.substring(0, 20) + '...'
                          : data.name
                        : '-'}
                    </span></td>
                  <td>
                    <span title={data?.description}>
                      {data.description
                        ? data.description.length > 20
                          ? data.description.substring(0, 20) + '...'
                          : data.description
                        : '-'}
                    </span>
                  </td>
                  <td>{formatBudgetValue(data?.budget ? data?.budget : 0)}</td>
                  <td><MoreVerticalIcon 
                  onClick={(e: any) => {
                    e.stopPropagation();
                    setOpenedContextMenuForSubCategory(
                      data.sub_category_id
                    );
                    setMoreIconDropdownOpen(
                      !moreIconDropdownOpen
                    );
                  }}
                  />
                  {moreIconDropdownOpen &&
                    data.sub_category_id ===
                      openedContextMenuForSubCategory && (
                      <ul className={Styles.menu}>
                        <li className={Styles.menuItem}>
                          <div
                            style={{
                              display: 'flex',
                              flexDirection: 'column',
                              gap: '5px',
                              padding: '5px',
                              backgroundColor:'#E5CFF7'
                            }}
                          >
                            <div
                              className={Styles.options}
                              onClick={() => {
                                navigate(`/bom/${data?.sub_category_id}`);
                              }}
                            >
                              <span className={Styles.menuFont}>
                                Manage Plan
                              </span>
                            </div>
                          </div>
                        </li>
                      </ul>
                    )}
                 </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
      <CustomSubCategoryAddPopup
        isVissible={showSubCategoryForm}
        onAction={setShowSubCategoryForm}
        selectedCategoryId={selectedCategory}
        selectedSubCategory={selectedSubCategoryId}
        selectedProject={props.projectsId}
        mode={mode}
        setMode={setMode}
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
  );
};

export default BomItems;
