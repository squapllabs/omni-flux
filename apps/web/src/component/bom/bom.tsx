import React, { useEffect, useState } from 'react';
import { useGetAllItemsDrops } from '../../hooks/item-hooks';
import { useGetAllUomDrop } from '../../hooks/uom-hooks';
import Styles from '../../styles/bom.module.scss';
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
import BomLabours from './bomTables/bomLabours';
import BomRawMaterials from './bomTables/bomRawMaterials';
import BomMachinery from './bomTables/bomMachinery';
import Button from '../ui/Button';
import CloseIcon from '../menu/icons/closeIcon';
import BackArrow from '../menu/icons/backArrow';

const Bom: React.FC = (props: any) => {
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
  const { data: getSubCategoryData } = getBySubcategoryID(
    Number(params?.subCategoryId)
  );
  console.log('getSubCategoryData', getSubCategoryData);

  useEffect(() => {
    const fetchData = async () => {
      console.log(
        'Number(params?.subCategoryId)',
        Number(params?.subCategoryId)
      );

      const getData = await BomService.getBOMbySubCatID(
        Number(params?.subCategoryId)
      );
      console.log('getData', getData);
      if (getData?.data != null) setBomList(getData?.data);
      const Rawmaterialobj = {
        id: params?.subCategoryId,
        type: 'RAWMT',
      };
      const getDataRawMAterial = await BomService.getBOMbySubCatIDandType(
        Rawmaterialobj
      );
      const labourobj = {
        id: params?.subCategoryId,
        type: 'LABOR',
      };
      const getDataLabour = await BomService.getBOMbySubCatIDandType(labourobj);
      const machineryobj = {
        id: params?.subCategoryId,
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
        console.log('getDataRawMAterial', sumOfRates);
      }
      if (getDatamachinery?.status === true) {
        const sumOfRates = await getDatamachinery?.data.reduce(
          (accumulator: any, currentItem: any) => {
            return accumulator + currentItem.total;
          },
          0
        );
        setMachineryTotal(sumOfRates);
        console.log('getDataRawMAterial', sumOfRates);
      }
    };
    fetchData();
  }, [activeButton, reload]);
  const { mutate: bulkBomData, data: responseData } = createBulkBom();
  const handleBulkBomAdd = () => {
    console.log('bomList', bomList);

    bulkBomData(bomList, {
      onSuccess(data, variables, context) {
        console.log('data', data);
        if (data?.status === true) {
          setMessage('BOM created successfully');
          setOpenSnack(true);
          setReload(!reload);
          setTimeout(() => {
            navigate(
              `/bomlist/${getSubCategoryData?.project_id}/${getSubCategoryData?.bom_configuration_id}`
            );
          }, 3000);
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
    <div>
      <div className={Styles.box}>
        <div className={Styles.mainTextContent}>
          <div className={Styles.textContent_1}>
            <h3>{getSubCategoryData?.project_data?.project_name}</h3>
            <span className={Styles.content}>
              {getSubCategoryData?.project_data?.description}
            </span>
          </div>
          <div className={Styles.backButton}>
            <div>
              <Button
                color="secondary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<BackArrow />}
                onClick={() => {
                  navigate(
                    `/bomlist/${getSubCategoryData?.project_id}/${getSubCategoryData?.bom_configuration_id}`
                  );
                }}
              >
                Back
              </Button>
            </div>
          </div>
        </div>
      </div>
      <div className={Styles.box}></div>
      <div className={Styles.bomcontainer}>
        <div className={Styles.headingcontainer}>
          {/* <div
          style={{
            paddingBottom: '10px',
            display: 'flex',
            justifyContent: 'flex-end',
          }}
        >
          <Button
            shape="rectangle"
            justify="center"
            size="small"
            icon={<BackArrow />}
            onClick={() => {
              navigate(
                `/bomlist/${getSubCategoryData?.project_id}/${getSubCategoryData?.bom_configuration_id}`
              );
            }}
          >
            Back
          </Button>
        </div> */}

          <div>
            <div className={Styles.mainHeading}>
              <div className={Styles.mainLeftContent}>
                <h3>{getSubCategoryData?.category?.name}</h3>
                <p className={Styles.descriptionContent}>
                  {getSubCategoryData?.category?.description}
                </p>
              </div>
              <div>
                <p>Allocated Budget</p>
                <p>
                  {formatBudgetValue(
                    getSubCategoryData?.category?.budget
                      ? getSubCategoryData?.category?.budget
                      : 0
                  )}
                </p>
              </div>
            </div>
          </div>
          <div>
            <div className={Styles.mainHeading}>
              <div className={Styles.mainLeftContent}>
                <h3>{getSubCategoryData?.name}</h3>
                <p className={Styles.descriptionContent}>
                  {getSubCategoryData?.description}
                </p>
              </div>
              <div>
                <p>Allocated Budget</p>
                <p>
                  {formatBudgetValue(
                    getSubCategoryData?.budget ? getSubCategoryData?.budget : 0
                  )}
                </p>
              </div>
            </div>
          </div>
        </div>
        <div className={Styles.groupButton}>
          <CustomGroupButton
            labels={buttonLabels}
            onClick={handleGroupButtonClick}
            activeButton={activeButton}
          />
        </div>
        <div className={Styles.mainBody}>
          {activeButton === 'RAWMT' ? (
            <BomRawMaterials
              subCategoryId={params.subCategoryId}
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
              subCategoryId={params.subCategoryId}
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
              subCategoryId={params.subCategoryId}
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

        <div className={Styles.totalPanel}>
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
        </div>
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </div>
    </div>
  );
};

export default Bom;
