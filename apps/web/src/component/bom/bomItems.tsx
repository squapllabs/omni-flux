import React from 'react';
import { getBycategoryIDinSub } from '../../hooks/subCategory-hooks';
import Button from '../ui/Button';
import Styles from '../../styles/bomList.module.scss';
import AddIcon from '../menu/icons/addIcon';
import { formatBudgetValue } from '../../helper/common-function';
const BomItems = (props: {
  selectedCategory: any;
  setSelectedSubCategory: any;
  selectedSubCategory: any;
}) => {
  console.log('daaa==>', props.selectedCategory);
  const { selectedCategory } = props;
  const { data: getAllData } = getBycategoryIDinSub(selectedCategory);
  console.log('getAllData in bom itemss', getAllData);

  return (
    <div>
      {getAllData?.map((items: any, index: any) => {
        return (
          <div>
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
                <p>{formatBudgetValue(items?.budget ? items?.budget : 0)}</p>
              </div>
            </div>
            <div className = {Styles.addPlan}onClick={() => {props.setSelectedSubCategory(items?.sub_category_id)}}>
            <AddIcon style={{ height: '15px', width: '15px' }} />
              <p>Add Plan</p>
            </div>
          </div>
        );
      })}
    </div>
  );
};

export default BomItems;

