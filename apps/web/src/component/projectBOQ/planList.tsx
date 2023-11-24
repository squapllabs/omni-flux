import React, { useEffect, useState } from 'react';
import { useGetAllItemsDrops } from '../../hooks/item-hooks';
import { useGetAllUomDrop } from '../../hooks/uom-hooks';
import Styles from '../../styles/newStyles/bomlist.module.scss';
import { useFormik } from 'formik';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import { useCreateBulkBom } from '../../hooks/bom-hooks';
import BomService from '../../service/bom-service';
import {
  getBombulkValidateyup,
  bomErrorMessages,
} from '../../helper/constants/bom-constants';
import * as Yup from 'yup';
import { useNavigate, useParams } from 'react-router-dom';
import { useGetBySubcategoryID } from '../../hooks/subCategory-hooks';
import { formatBudgetValue } from '../../helper/common-function';
import BomLabours from './boqTables/boqLabours';
import BomRawMaterials from './boqTables/boqRawMaterials';
import BomMachinery from './boqTables/boqMachinery';
import Button from '../ui/Button';
import CloseIcon from '../menu/icons/closeIcon';
import BackArrow from '../menu/icons/backArrow';
import CustomSidePopup from '../ui/CustomSidePopup';
import InstantItemAdd from '../ui/CustomItemAdd';
import InstantLabourAdd from '../ui/CustomLabourAdd';
import InstantMachineryAdd from '../ui/CustomMachineryAdd';
/* BOQ plan list screen */
const Bom: React.FC = (props: any) => {
  const subCategoryId = Number(props.subCategoryId);
  const subCategory = props.subCategory;
  const params = useParams();
  const navigate = useNavigate();
  const [bomList, setBomList] = useState<any[]>([]);
  const [itemForm, showItemForm] = useState(false);
  const [labourForm, showLabourForm] = useState(false);
  const [machineryForm, showMachineryForm] = useState(false);
  const [rawMaterialTotal, setRawMaterialTotal] = useState(0);
  const [labourTotal, setRawLabourTotal] = useState(0);
  const [machineryTotal, setMachineryTotal] = useState(0);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'RAW MATERIAL', value: 'RAWMT' },
    { label: 'LABOUR', value: 'LABOR' },
    { label: 'MACHINERY', value: 'MCNRY' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('RAWMT');
  const [reload, setReload] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [errors, setErrors] = useState<any>();
  const [openDialog, setOpenDialog] = useState(false);
  const { data: getSubCategoryData } = useGetBySubcategoryID(subCategoryId);
  const [bomTotal, setBomTotal] = useState<number>(0);
  /* Function to get bom list */
  useEffect(() => {
    const fetchData = async () => {
      const getData = await BomService.getBOMbySubCatID(subCategoryId);
      if (getData?.data != null) setBomList(getData?.data);
      const Rawmaterialobj = {
        id: subCategoryId,
        type: 'RAWMT',
      };
      const getDataRawMAterial = await BomService.getBOMbySubCatIDandType(
        Rawmaterialobj
      );
      const labourobj = {
        id: subCategoryId,
        type: 'LABOR',
      };
      const getDataLabour = await BomService.getBOMbySubCatIDandType(labourobj);
      const machineryobj = {
        id: subCategoryId,
        type: 'MCNRY',
      };
      const getDatamachinery = await BomService.getBOMbySubCatIDandType(
        machineryobj
      );
      if (getDataRawMAterial?.status === true) {
        const sumOfRates = await getDataRawMAterial?.data.reduce(
          (accumulator: any, currentItem: any) => {
            return accumulator + currentItem.total;
          },
          0
        );
        setRawMaterialTotal(sumOfRates);
      }
      if (getDataLabour?.status === true) {
        const sumOfRates = await getDataLabour?.data.reduce(
          (accumulator: any, currentItem: any) => {
            return accumulator + currentItem.total;
          },
          0
        );
        setRawLabourTotal(sumOfRates);
      }
      if (getDatamachinery?.status === true) {
        const sumOfRates = await getDatamachinery?.data.reduce(
          (accumulator: any, currentItem: any) => {
            return accumulator + currentItem.total;
          },
          0
        );
        setMachineryTotal(sumOfRates);
      }
    };
    fetchData();
  }, [activeButton, reload]);
  useEffect(() => {
    let total_value = 0;
    if (bomList.length) {
      bomList.forEach((element) => {
        const rate = Number(element.rate);
        const quantity = Number(element.quantity);
        const is_delete = element.is_delete;
        if (rate && quantity && !is_delete) {
          total_value = total_value + rate * quantity;
        }
      });
    }
    setBomTotal(total_value);
  }, [bomList, activeButton, reload]);
  const { mutate: bulkBomData, data: responseData } = useCreateBulkBom();
  const validationSchema = Yup.array().of(
    Yup.object()
      .shape({
        uom_id: Yup.string().trim().required('UOM is required'),
        rate: Yup.number()
          .required('Rate is required')
          .typeError('Numbers only allowed')
          .min(1),
        quantity: Yup.number()
          .required('Quantity is required')
          .typeError('Numbers only allowed')
          .min(1),
        item_id: Yup.string()
          .trim()
          .nullable()
          .test(
            'decimal-validation',
            'Already Exists',
            async function (value, { parent }: Yup.TestContext) {
              if (value != null) {
                try {
                  const bOMType = parent.bom_type;
                  if (bOMType === 'RAWMT') {
                    const dummy: any = [];
                    const allIds = bomList.map((item: any) => {
                      if (item.is_delete === false) {
                        dummy.push(item.item_id);
                      }
                    });
                    const checking = dummy.filter(
                      (id: any) => Number(id) === Number(value)
                    ).length;
                    if (checking <= 1) {
                      return true;
                    } else return false;
                  } else {
                    return false;
                  }
                } catch {
                  return true;
                }
              } else {
                return true;
              }
            }
          ),
        labour_id: Yup.string()
          .trim()
          .nullable()
          .test(
            'decimal-validation',
            'Already Exists',
            async function (value, { parent }: Yup.TestContext) {
              if (value != null) {
                try {
                  const bOMType = parent.bom_type;
                  if (bOMType === 'LABOR') {
                    const dummy: any = [];
                    const allIds = bomList.map((item: any) => {
                      if (item.is_delete === false) {
                        dummy.push(item.labour_id);
                      }
                    });
                    const checking = dummy.filter(
                      (id: any) => Number(id) === Number(value)
                    ).length;
                    if (checking <= 1) {
                      return true;
                    } else return false;
                  } else {
                    return false;
                  }
                } catch {
                  return true;
                }
              } else {
                return true;
              }
            }
          ),
        machinery_id: Yup.string()
          .trim()
          .nullable()
          .test(
            'decimal-validation',
            'Already Exists',
            async function (value, { parent }: Yup.TestContext) {
              if (value != null) {
                try {
                  const bOMType = parent.bom_type;
                  if (bOMType === 'MCNRY') {
                    const dummy: any = [];
                    const allIds = bomList.map((item: any) => {
                      if (item.is_delete === false) {
                        dummy.push(item.machinery_id);
                      }
                    });
                    const checking = dummy.filter(
                      (id: any) => Number(id) === Number(value)
                    ).length;
                    if (checking <= 1) {
                      return true;
                    } else return false;
                  } else {
                    return false;
                  }
                } catch {
                  return true;
                }
              } else {
                return true;
              }
            }
          ),
      })
      .test('at-least-one-required', null, (values) => {
        const { labour_id, item_id, machinery_id } = values;
        if (!labour_id && !item_id && !machinery_id) {
          return new Yup.ValidationError(
            'At least one of labour_id, item_id, machenery_id is required',
            null,
            'at-least-one-required'
          );
        }
        return true;
      })
  );
  /* Function to bulk upload bom data */
  const handleBulkBomAdd = () => {
    validationSchema
      .validate(bomList, { abortEarly: false })
      .then(() => {
        bulkBomData(bomList, {
          onSuccess(data, variables, context) {
            if (data?.status === true) {
              setMessage('BOM details added successfully');
              setOpenSnack(true);
              setReload(!reload);
              setTimeout(() => {
                props.setOpen(!props.open);
                props.setReload(!props.reload);
                props.setSubTaskView(!props.subTaskView);
                props.setIsCollapsed(!props.isCollapsed);
              }, 1000);
            }
          },
        });
      })
      .catch((e: any) => {
        const errorObj = {};
        e.inner?.map((error: any) => {
          return (errorObj[error.path] = error.message);
        });
        setErrors({
          ...errorObj,
        });
      });
  };
  const handleClose = () => {
    props.setOpen(false);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
    bomList.map((item: any, index: any) => {
      if (
        item.labour_id === '' ||
        item.item_id === '' ||
        item.machinery_id === ''
      ) {
        bomList.splice(index, 1);
      }
    });
  };
  const handleItemFormClose = () => {
    showItemForm(false);
  };
  const handleLabourFormClose = () => {
    showLabourForm(false);
  };
  const handleMachineryFormClose = () => {
    showMachineryForm(false);
  };
  const handleCloseDialog = () => {
    setOpenDialog(false);
  };

  return (
    <div className={Styles.bomcontainer}>
      <div className={Styles.sub_container}>
        <div className={Styles.sub_sub_container}>
          <div
            style={{
              display: 'flex',
              flexDirection: 'row',
              justifyContent: 'space-between',
            }}
          >
            <div style={{ paddingLeft: '10px' }}>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
            </div>
            <div
              className={Styles.countContent}
              style={{
                display: 'flex',
                flexDirection: 'row',
                justifyContent: 'space-between',
                gap: '2rem',
              }}
            >
              <div>
                <h3>{formatBudgetValue(subCategory?.estimated_budget)}</h3>
                <span className={Styles.countContentTitle}>
                  Estimated Budget
                </span>
              </div>
              <div
                className={
                  subCategory?.estimated_budget < bomTotal ? 'danger' : ''
                }
              >
                <h3
                  style={{
                    color:
                      subCategory?.estimated_budget < bomTotal ? 'red' : '',
                  }}
                >
                  {formatBudgetValue(bomTotal)}
                </h3>
                <span
                  style={{
                    color:
                      subCategory?.estimated_budget < bomTotal ? 'red' : '',
                  }}
                  className={Styles.countContentTitle}
                >
                  Total Amount
                </span>
              </div>
            </div>
          </div>
          <div className={Styles.mainBody}>
            {activeButton === 'RAWMT' ? (
              <BomRawMaterials
                subCategoryId={subCategoryId}
                activeButton={activeButton}
                projectId={getSubCategoryData?.project_id}
                bomId={getSubCategoryData?.bom_configuration_id}
                setRawMaterialTotal={setRawMaterialTotal}
                rawMaterialTotal={rawMaterialTotal}
                setReload={setReload}
                reload={reload}
                setBomList={setBomList}
                bomList={bomList}
                showItemForm={showItemForm}
                errors={errors}
                setErrors={setErrors}
              />
            ) : (
              ''
            )}
            {activeButton === 'LABOR' ? (
              <BomLabours
                subCategoryId={subCategoryId}
                activeButton={activeButton}
                projectId={getSubCategoryData?.project_id}
                bomId={getSubCategoryData?.bom_configuration_id}
                setRawMaterialTotal={setRawMaterialTotal}
                rawMaterialTotal={rawMaterialTotal}
                setReload={setReload}
                reload={reload}
                setBomList={setBomList}
                bomList={bomList}
                showLabourForm={showLabourForm}
                errors={errors}
                setErrors={setErrors}
              />
            ) : (
              ''
            )}
            {activeButton === 'MCNRY' ? (
              <BomMachinery
                subCategoryId={subCategoryId}
                activeButton={activeButton}
                projectId={getSubCategoryData?.project_id}
                bomId={getSubCategoryData?.bom_configuration_id}
                setRawMaterialTotal={setRawMaterialTotal}
                rawMaterialTotal={rawMaterialTotal}
                setReload={setReload}
                reload={reload}
                setBomList={setBomList}
                bomList={bomList}
                showMachineryForm={showMachineryForm}
                errors={errors}
                setErrors={setErrors}
              />
            ) : (
              ''
            )}
          </div>
        </div>
        <div className={Styles.sub_sub_container_2}>
          <div className={Styles.footer}>
            <div>
              <div className={Styles.dividerStyle}></div>
              <div className={Styles.button}>
                <Button
                  shape="rectangle"
                  justify="center"
                  size="small"
                  onClick={handleClose}
                  color='cancel'
                  // className={Styles.cancelButton}
                >
                  Cancel
                </Button>
                <Button
                  shape="rectangle"
                  color="primary"
                  justify="center"
                  size="small"
                  type="submit"
                  onClick={(e) => handleBulkBomAdd(e)}
                >
                  Save
                </Button>
              </div>
            </div>
          </div>
        </div>
      </div>
      <CustomSidePopup
        open={itemForm}
        title="Add Item"
        handleClose={handleItemFormClose}
        content={
          <InstantItemAdd
            isVissible={itemForm}
            onAction={showItemForm}
            setMessage={setMessage}
            setOpenSnack={setOpenSnack}
          />
        }
        width="90%"
        description=""
      />
      <CustomSidePopup
        open={labourForm}
        title="Add Labour"
        handleClose={handleLabourFormClose}
        content={
          <InstantLabourAdd
            isVissible={labourForm}
            onAction={showLabourForm}
            setMessage={setMessage}
            setOpenSnack={setOpenSnack}
          />
        }
        width="50%"
        description=""
      />
      <CustomSidePopup
        open={machineryForm}
        title="Add Machinery"
        handleClose={handleMachineryFormClose}
        content={
          <InstantMachineryAdd
            isVissible={machineryForm}
            onAction={showMachineryForm}
            setMessage={setMessage}
            setOpenSnack={setOpenSnack}
          />
        }
        width="90%"
        description=""
      />
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default Bom;

