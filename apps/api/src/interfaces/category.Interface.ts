interface createCategoryBody {
  name: string;
  project_id: number;
  budget: number;
  created_by: bigint;
  description: string;
  start_date: Date;
  end_date: Date;
  bom_configuration_id: number;
}

interface updateCategoryBody {
  name: string;
  project_id: number;
  budget: number;
  updated_by: bigint;
  category_id: number;
  description: string;
  start_date: Date;
  end_date: Date;
  bom_configuration_id: number;
}

export { createCategoryBody, updateCategoryBody };
