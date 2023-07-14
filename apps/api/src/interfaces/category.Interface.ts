interface createCategoryBody {
  name: string;
  project_id: number;
  budget: number;
  created_by: bigint;
}

interface updateCategoryBody {
  name: string;
  project_id: number;
  budget: number;
  updated_by: bigint;
  category_id: number;
}

export { createCategoryBody, updateCategoryBody };
