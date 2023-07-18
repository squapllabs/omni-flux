interface createProductBody{
  product_name: string;
  sub_sub_category_id: number;
  description: string;
  hsn_code_id:number;
  gst_id:number;
  uom_id:number;
  created_by:bigint;
  updated_by:bigint;
}
interface updateProductBody{
  product_id:number;
  product_name: string;
  sub_sub_category_id: number;
  description: string;
  hsn_code_id:number;
  gst_id:number;
  uom_id:number;
  updated_by:bigint;
}

export {createProductBody,updateProductBody
}