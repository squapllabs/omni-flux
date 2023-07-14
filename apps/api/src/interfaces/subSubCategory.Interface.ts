interface createSubSubCategoryBody {
  name: string;
  sub_category_id: number;
  budget: number;
  created_by: bigint;
}

interface updateSubSubCategoryBody {
  name: string;
  sub_category_id: number;
  budget: number;
  updated_by: bigint;
  sub_sub_category_id: number;
}

export { createSubSubCategoryBody, updateSubSubCategoryBody };
