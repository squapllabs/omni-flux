import React, { useState } from 'react';
import {
  getBycategoryIDinSub,
  useDeleteSubcategory,
} from '../../hooks/subCategory-hooks';
import Button from '../ui/Button';
import Styles from '../../styles/bomList.module.scss';
import AddIcon from '../menu/icons/addIcon';
import { formatBudgetValue } from '../../helper/common-function';
import EditIcon from '../menu/icons/editIcon';
import CustomSubCategoryAddPopup from '../ui/CustomSubCategoryPopup';
import DeleteIcon from '../menu/icons/deleteIcon';
import CustomSnackBar from '../ui/customSnackBar';
import CustomDelete from '../ui/customDeleteDialogBox';

const BomItems = (props: {
  selectedCategory: any;
  setSelectedSubCategory: any;
  selectedSubCategory: any;
  selectedProject: any;
}) => {
  const { selectedCategory, selectedProject, selectedSubCategory } = props;
  const { data: getAllData } = getBycategoryIDinSub(selectedCategory);
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

  return (
    <div className={Styles.scrollContainer}>
      <div>
        {getAllData?.map((items: any, index: any) => {
          return (
            <div>
              <div className={Styles.dividerContent}>
                <div className={Styles.mainHeading}>
                  <div className={Styles.mainLeftContent}>
                    <h4>
                      {index + 1}. {items?.name}
                    </h4>
                    <p className={Styles.descriptionContent}>
                      {items?.description}
                    </p>
                  </div>
                  <div className={Styles.rightContent}>
                    <p>
                      {formatBudgetValue(items?.budget ? items?.budget : 0)}
                    </p>
                  </div>
                </div>
                <div className={Styles.options}>
                  <div
                    className={Styles.addPlan}
                    onClick={() => {
                      props.setSelectedSubCategory(items?.sub_category_id);
                    }}
                  >
                    <AddIcon style={{ height: '15px', width: '15px' }} />
                    <p className={Styles.addText}>Add Plan</p>
                  </div>
                  <div
                    className={Styles.addPlan}
                    onClick={() => {
                      // setShowSubCategoryForm(items?.sub_category_id);
                      handleEdit(items?.sub_category_id);
                      setSelectedSubCategoryId(items?.sub_category_id);
                    }}
                  >
                    <EditIcon style={{ height: '15px', width: '15px' }} />
                    <p className={Styles.addText}>Edit Task</p>
                  </div>
                  <div
                    className={Styles.addPlan}
                    onClick={() => {
                      deleteHandler(items?.sub_category_id);
                    }}
                  >
                    <DeleteIcon style={{ height: '15px', width: '15px' }} />
                    <p className={Styles.addText}>Delete Task</p>
                  </div>
                </div>
              </div>
            </div>
          );
        })}
      </div>
      <CustomSubCategoryAddPopup
        isVissible={showSubCategoryForm}
        onAction={setShowSubCategoryForm}
        selectedCategoryId={selectedCategory}
        selectedSubCategory={selectedSubCategoryId}
        selectedProject={selectedProject}
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
