interface bomBody {
  bom_detail_id: number;
  bom_name: string;
  quantity: number;
  uom_id: number;
  category_id: number;
  sub_category_id: number;
  sub_sub_category_id: number;
  item_id: number;
  is_delete: boolean;
  created_by: number;
  updated_by: number;
  description: string;
  rate: number;
  total: number;
  bom_type: string;
  created_date: Date;
  updated_date: Date;
  machinery_id: number;
  labour_id: number;
  bom_configuration_id: number;
}

export { bomBody };
