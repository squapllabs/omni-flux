interface createSubSubCategoryBody {
  name: string;
  sub_category_id: number;
  budget: number;
  created_by: bigint;
  description: string;
}

interface updateSubSubCategoryBody {
  name: string;
  sub_category_id: number;
  budget: number;
  updated_by: bigint;
  sub_sub_category_id: number;
  description: string;
}

export { createSubSubCategoryBody, updateSubSubCategoryBody };
