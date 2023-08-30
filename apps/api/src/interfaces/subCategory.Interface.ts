interface createSubCategoryBody {
  name: string;
  category_id: number;
  budget: number;
  created_by: bigint;
  description: string;
  project_id: number;
}

interface updateSubCategoryBody {
  name: string;
  category_id: number;
  budget: number;
  updated_by: bigint;
  sub_category_id: number;
  description: string;
  project_id: number;
}

export { createSubCategoryBody, updateSubCategoryBody };
