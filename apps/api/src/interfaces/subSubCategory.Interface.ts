interface createSubSubCategoryBody {
  name: string;
  sub_category_id: number;
  budget: number;
  created_by: bigint;
  description: string;
  project_id: number;
  parent_sub_sub_category_id: number;
}

interface updateSubSubCategoryBody {
  name: string;
  sub_category_id: number;
  budget: number;
  updated_by: bigint;
  sub_sub_category_id: number;
  description: string;
  project_id: number;
  parent_sub_sub_category_id: number;
}

export { createSubSubCategoryBody, updateSubSubCategoryBody };
