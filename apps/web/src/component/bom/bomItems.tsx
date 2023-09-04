import React from 'react';
import { getBycategoryIDinSub } from '../../hooks/subCategory-hooks';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
const BomItems: React.FC = (props: any) => {
  const { data: getAllData } = getBycategoryIDinSub(props.selectedCategory);
  console.log('getAllData', getAllData);

  return (
    <div>
      {getAllData?.map((items: any, index: any) => {
        return (
          <div>
            <div>{items?.description}</div>
            <Button
              color="primary"
              shape="rectangle"
              justify="center"
              size="small"
              icon={<AddIcon width={20} />}
              onClick={() => {
                props.setSelectedSubCategory(items?.sub_category_id);
              }}
            >
              Add
            </Button>
          </div>
        );
      })}
    </div>
  );
};

export default BomItems;
