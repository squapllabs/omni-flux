import React, { useEffect, useState } from 'react';
import { getBycategoryIdInSub } from '../../hooks/subCategory-hooks';
import Styles from '../../styles/bomList.module.scss';
import AddIcon from '../menu/icons/addIcon';
import { formatBudgetValue } from '../../helper/common-function';
import { useNavigate } from 'react-router-dom';
import bomService from '../../service/bom-service';
import CustomGroupButton from '../ui/CustomGroupButton';

const BomItems = (props: {
  selectedCategory: any;
  setSelectedSubCategory: any;
  selectedSubCategory: any;
  projectsId: any;
}) => {
  const { selectedCategory, projectsId } = props;
  const { data: getAllData } = getBycategoryIdInSub(selectedCategory);
  const navigate = useNavigate();
  const [isExpanded, setIsExpanded] = useState(null);
  const [tableData, setTableData] = useState();
  const [activeButton, setActiveButton] = useState<string | null>('RAWMT');
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'RAW MATERIAL', value: 'RAWMT' },
    { label: 'LABOUR', value: 'RAWLB' },
    { label: 'MACHINERY', value: 'MAC' },
  ]);
  console.log('getAllData in bom itemss', getAllData);

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  const handleDemo = async (subCategoryId: any) => {
    const obj = {
      id: subCategoryId,
      type: activeButton,
    };
    if (isExpanded === subCategoryId) {
      setIsExpanded(null);
    } else {
      const getData = await bomService.getBOMbySubCatIDandType(obj);
      console.log("sample data =====>",getData.data);
      
      setTableData(getData.data);
      setIsExpanded(subCategoryId);
    }
  };

  return (
    <div className={Styles.scrollContainer}>
      <div>
        {getAllData?.map((items: any, index: any) => {
          const isItemExpanded = isExpanded === items.sub_category_id;
          return (
            <div key={items.sub_category_id}>
              <div className={Styles.dividerContent}>
                <div className={Styles.mainHeading}>
                  <div
                    className={Styles.mainLeftContent}
                    onClick={() => handleDemo(items?.sub_category_id)}
                  >
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
                      <table>
                        <thead>
                          <tr>
                            <th>ITEM</th>
                            <th>UOM</th>
                            <th>QUANTITY</th>
                            <th>RATE</th>
                            <th>Total</th>
                          </tr>
                        </thead>
                        <tbody>
                          {tableData?.map((item: any, index: any) => (
                            <tr key={item.bom_id}>
                              <td>{item.item_data?.item_name}</td>
                              <td>{item.uom_data?.name}</td>
                              <td>{item.quantity}</td>
                              <td>{formatBudgetValue(item.rate)}</td>
                              <td>{formatBudgetValue(item.total)}</td>
                            </tr>
                          ))}
                        </tbody>
                      </table>
                    </div>
                  )}
                </div>
                <div
                  className={Styles.addPlan}
                  onClick={() => {
                    navigate(
                      `/bom/${items?.sub_category_id}/${props.projectsId}`
                    );
                  }}
                >
                  <AddIcon style={{ height: '15px', width: '15px' }} />
                  <p className={Styles.addText}>Add Plan</p>
                </div>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
};

export default BomItems;
