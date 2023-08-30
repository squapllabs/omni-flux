interface bomBody {
  bom_id: number;
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
}

export { bomBody };
