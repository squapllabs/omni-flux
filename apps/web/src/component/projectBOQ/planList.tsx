import React, { useEffect, useState } from 'react';
import { useGetAllItemsDrops } from '../../hooks/item-hooks';
import { useGetAllUomDrop } from '../../hooks/uom-hooks';
import Styles from '../../styles/newStyles/bomlist.module.scss';
import { useFormik } from 'formik';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import { createBulkBom } from '../../hooks/bom-hooks';
import BomService from '../../service/bom-service';
import {
  getBombulkValidateyup,
  bomErrorMessages,
} from '../../helper/constants/bom-constants';
import * as Yup from 'yup';
import { useNavigate, useParams } from 'react-router-dom';
import { getBySubcategoryID } from '../../hooks/subCategory-hooks';
import { formatBudgetValue } from '../../helper/common-function';
import BomLabours from './boqTables/boqLabours';
import BomRawMaterials from './boqTables/boqRawMaterials';
import BomMachinery from './boqTables/boqMachinery';
import Button from '../ui/Button';
import CloseIcon from '../menu/icons/closeIcon';
import BackArrow from '../menu/icons/backArrow';

const Bom: React.FC = (props: any) => {
  const subCategoryId = Number(props.subCategoryId);
  const params = useParams();
  const navigate = useNavigate();
  const [bomList, setBomList] = useState<any[]>([]);
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
  const { data: getSubCategoryData } = getBySubcategoryID(subCategoryId);

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
  const { mutate: bulkBomData, data: responseData } = createBulkBom();
  const handleBulkBomAdd = () => {
    bulkBomData(bomList, {
      onSuccess(data, variables, context) {
        if (data?.status === true) {
          setMessage('BOM created successfully');
          setOpenSnack(true);
          setReload(!reload);
          props.setOpen(!props.open);
        }
      },
    });
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  return (
    <div className={Styles.bomcontainer}>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          justifyContent: 'space-between',
        }}
      >
        <div>
          <CustomGroupButton
            labels={buttonLabels}
            onClick={handleGroupButtonClick}
            activeButton={activeButton}
          />
        </div>
        <div className={Styles.countContent}>
          <h3>
            {formatBudgetValue(rawMaterialTotal + labourTotal + machineryTotal)}
          </h3>
          <span className={Styles.countContentTitle}>Aggregated Value</span>
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
          />
        ) : (
          ''
        )}
        <div className={Styles.saveButton}>
          <Button
            color="primary"
            shape="rectangle"
            justify="center"
            size="small"
            onClick={(e) => handleBulkBomAdd(e)}
          >
            SAVE
          </Button>
        </div>
      </div>

      {/* <div className={Styles.totalPanel}>
        <div className={Styles.panelList}>
          <div className={Styles.panel}>
            <span className={Styles.panelTitle}>Raw Material Cost:</span>
            <span>{rawMaterialTotal}</span>
          </div>
          <div className={Styles.panel}>
            <span className={Styles.panelTitle}>Manpower Cost:</span>
            <span>{labourTotal}</span>
          </div>
          <div className={Styles.panel}>
            <span className={Styles.panelTitle}>Machinery Cost:</span>
            <span>{machineryTotal}</span>
          </div>
          <div className={Styles.panel}>
            <span className={Styles.panelTitle}>Total:</span>
            <span>{rawMaterialTotal + labourTotal + machineryTotal}</span>
          </div>
        </div>
      </div> */}
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
