interface createItemBody{
  item_name: string;
  sub_sub_category_id: number;
  description: string;
  hsn_code_id:number;
  gst_id:number;
  uom_id:number;
  created_by:bigint;
  updated_by:bigint;
  item_type_id:number;
  brand_id:number;
}
interface updateItemBody{
  item_id:number;
  item_name: string;
  sub_sub_category_id: number;
  description: string;
  hsn_code_id:number;
  gst_id:number;
  uom_id:number;
  updated_by:bigint;
  item_type_id:number;
  brand_id:number;
}

export {createItemBody,updateItemBody}