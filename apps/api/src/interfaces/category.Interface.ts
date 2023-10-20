interface createCategoryBody {
  name: string;
  project_id: number;
  actual_budget: number;
  estimated_budget: number;
  created_by: bigint;
  description: string;
  start_date: Date;
  end_date: Date;
  bom_configuration_id: number;
  progress_status: string;
}

interface updateCategoryBody {
  name: string;
  project_id: number;
  actual_budget: number;
  estimated_budget: number;
  updated_by: bigint;
  category_id: number;
  description: string;
  start_date: Date;
  end_date: Date;
  bom_configuration_id: number;
  progress_status: string;
}

export { createCategoryBody, updateCategoryBody };
