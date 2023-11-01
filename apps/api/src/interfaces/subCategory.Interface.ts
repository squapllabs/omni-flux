interface createSubCategoryBody {
  children: Array<createSubCategoryBody>;
  name: string;
  category_id: number;
  actual_budget: number;
  created_by: bigint;
  description: string;
  project_id: number;
  start_date: Date;
  end_date: Date;
  bom_configuration_id: number;
  progress_status: string;
  parent_sub_category_id: number;
  estimated_budget: number;
  uom_id: number;
  quantity: number;
  rate: number;
}

interface updateSubCategoryBody {
  name: string;
  category_id: number;
  actual_budget: number;
  updated_by: bigint;
  sub_category_id: number;
  description: string;
  project_id: number;
  start_date: Date;
  end_date: Date;
  bom_configuration_id: number;
  progress_status: string;
  parent_sub_category_id: number;
  estimated_budget: number;
  uom_id: number;
  quantity: number;
  rate: number;
}

export { createSubCategoryBody, updateSubCategoryBody };
