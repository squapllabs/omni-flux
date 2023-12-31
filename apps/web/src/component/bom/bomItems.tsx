import React, { useEffect, useState } from 'react';
import {
  useGetBycategoryIdInSub,
  useDeleteSubcategory,
} from '../../hooks/subCategory-hooks';
import Styles from '../../styles/bomList.module.scss';
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
import ProjectTaskAdd from '../projectBOQ/forms/ProjectTaskAdd';
import CustomSidePopup from '../ui/CustomSidePopup';

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
  const { data: getAllData } = useGetBycategoryIdInSub(obj);
  const { mutate: getDeleteSubCategoryByID } = useDeleteSubcategory();
  const [showSubCategoryForm, setShowSubCategoryForm] = useState(false);
  const [selectedSubCategoryId, setSelectedSubCategoryId] = useState();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [isWarning, setIswarning] = useState(false);
  const [mode, setMode] = useState('');
  const [reload, setReload] = useState(false);
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [value, setValue] = useState();
  const navigate = useNavigate();
  const [isExpanded, setIsExpanded] = useState(null);
  const [tableData, setTableData] = useState();
  const [istableLoader, setIsTableLoader] = useState(true);
  const [activeButton, setActiveButton] = useState<string | null>('RAWMT');
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'RAW MATERIAL', value: 'RAWMT' },
    { label: 'LABOUR', value: 'LABOR' },
    { label: 'MACHINERY', value: 'MCNRY' },
  ]);

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
  const handleGroupButtonClick = async (value: string) => {
    setIsTableLoader(true);
    setActiveButton(value);
    const obj = {
      id: isExpanded,
      type: value,
    };
    try {
      const getData = await bomService.getBOMbySubCatIDandType(obj);
      setTableData(getData.data);
      setIsTableLoader(false);
    } catch (error) {
      console.error('Error fetching data in handleGroupButtonClick :', error);
    }
  };

  const handleDemo = async (subCategoryId: any) => {
    const obj = {
      id: subCategoryId,
      type: activeButton,
    };
    if (isExpanded === subCategoryId) {
      setIsExpanded(null);
      setActiveButton('RAWMT');
    } else {
      const getData = await bomService.getBOMbySubCatIDandType(obj);
      setActiveButton('RAWMT');
      setTableData(getData.data);
      setIsExpanded(subCategoryId);
      setIsTableLoader(false);
    }
  };

  useEffect(() => {
    // handleDemo()
  }, [activeButton]);

  const handleCloseTask = () => {
    setShowSubCategoryForm(false);
  };

  return (
    <div className={Styles.scrollContainer}>
      <div>
        {getAllData?.map((items: any, index: any) => {
          const isItemExpanded = isExpanded === items.sub_category_id;
          return (
            <div key={items.sub_category_id}>
              <div className={Styles.dividerContent}>
                <div
                  className={Styles.mainHeading}
                  onClick={() => handleDemo(items?.sub_category_id)}
                >
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
                <div>
                  {isItemExpanded && (
                    <div>
                      <div className={Styles.groupButton}>
                        <CustomGroupButton
                          labels={buttonLabels}
                          onClick={handleGroupButtonClick}
                          activeButton={activeButton}
                        />
                      </div>
                      <div>
                        {istableLoader ? (
                          <CustomLoader loading={istableLoader} size={48} />
                        ) : activeButton === 'RAWMT' ? (
                          <table>
                            <thead>
                              <tr>
                                <th>S No</th>
                                <th>ITEM</th>
                                <th>UOM</th>
                                <th>QUANTITY</th>
                                <th>RATE</th>
                                <th>Total</th>
                              </tr>
                            </thead>
                            <tbody>
                              {tableData && tableData.length > 0 ? (
                                tableData.map((item: any, index: any) => (
                                  <tr key={item.bom_detail_id}>
                                    <td>{index + 1}</td>
                                    <td>{item.item_data?.item_name}</td>
                                    <td>{item.uom_data?.name}</td>
                                    <td>{item.quantity}</td>
                                    <td>{formatBudgetValue(item.rate)}</td>
                                    <td>{formatBudgetValue(item.total)}</td>
                                  </tr>
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
                        ) : (
                          ' '
                        )}
                        {istableLoader ? (
                          <CustomLoader loading={istableLoader} size={48} />
                        ) : activeButton === 'LABOR' ? (
                          <table>
                            <thead>
                              <tr>
                                <th>S No</th>
                                <th>LABOUR TYPE</th>
                                <th>WAGES TYPE</th>
                                <th>LABOUR Count</th>
                                <th>RATE</th>
                                <th>Total</th>
                              </tr>
                            </thead>
                            <tbody>
                              {tableData && tableData.length > 0 ? (
                                tableData.map((item: any, index: any) => (
                                  <tr key={item.bom_detail_id}>
                                    <td>{index + 1}</td>
                                    <td>{item.bom_name}</td>
                                    <td>{item.uom_data?.name}</td>
                                    <td>{item.quantity}</td>
                                    <td>{formatBudgetValue(item.rate)}</td>
                                    <td>{formatBudgetValue(item.total)}</td>
                                  </tr>
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
                        ) : (
                          ' '
                        )}
                        {istableLoader ? (
                          <CustomLoader loading={istableLoader} size={48} />
                        ) : activeButton === 'MCNRY' ? (
                          <table>
                            <thead>
                              <tr>
                                <th>S No</th>
                                <th>MACHINE TYPE</th>
                                <th>RENT TYPE</th>
                                <th>MACHINE Count</th>
                                <th>RATE</th>
                                <th>Total</th>
                              </tr>
                            </thead>
                            <tbody>
                              {tableData && tableData.length > 0 ? (
                                tableData.map((item: any, index: any) => (
                                  <tr key={item.bom_detail_id}>
                                    <td>{index + 1}</td>
                                    <td>{item.bom_name}</td>
                                    <td>{item.uom_data?.name}</td>
                                    <td>{item.quantity}</td>
                                    <td>{formatBudgetValue(item.rate)}</td>
                                    <td>{formatBudgetValue(item.total)}</td>
                                  </tr>
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
                        ) : (
                          ' '
                        )}
                      </div>
                    </div>
                  )}
                </div>
                <div className={Styles.options}>
                  <div
                    className={Styles.addPlan}
                    onClick={() => {
                      navigate(`/bom/${items?.sub_category_id}`);
                    }}
                  >
                    <AddIcon style={{ height: '15px', width: '15px' }} />
                    <p className={Styles.addText}>Add Plan</p>
                  </div>
                  <div
                    className={Styles.addPlan}
                    onClick={() => {
                      handleEdit(items?.sub_category_id);
                      setSelectedSubCategoryId(items?.sub_category_id);
                    }}
                  >
                    <EditIcon style={{ height: '15px', width: '15px' }} />
                    <p className={Styles.addText}>Edit Task</p>
                  </div>
                  {/* <div
                    className={Styles.addPlan}
                    onClick={() => {
                      deleteHandler(items?.sub_category_id);
                    }}
                  >
                    <DeleteIcon style={{ height: '15px', width: '15px' }} />
                    <p className={Styles.addText}>Delete Task</p>
                  </div> */}
                </div>
                {/* <div
                  className={Styles.addPlan}
                  onClick={() => {
                    navigate(
                      `/bom/${items?.sub_category_id}/${selectedProject}`
                    );
                  }}
                >
                  <AddIcon style={{ height: '15px', width: '15px' }} />
                  <p className={Styles.addText}>Add Plan</p>
                </div> */}
              </div>
            </div>
          );
        })}
      </div>
      <CustomSidePopup
        open={showSubCategoryForm}
        title={mode === 'EDIT' ? 'Edit Task' : 'Create New Task'}
        handleClose={handleCloseTask}
        content={
          <ProjectTaskAdd
            open={showSubCategoryForm}
            setOpen={setShowSubCategoryForm}
            selectedProject={props.projectsId}
            reload={reload}
            setReload={setReload}
            openSnack={openSnack}
            setOpenSnack={setOpenSnack}
            message={message}
            setMessage={setMessage}
            mode={mode}
            selectedCategoryId={selectedCategory}
            selectedSubCategory={selectedSubCategoryId}
          />
        }
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
