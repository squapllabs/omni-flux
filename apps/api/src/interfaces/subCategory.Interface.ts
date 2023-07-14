interface createSubCategoryBody {
  name: string;
  category_id: number;
  budget: number;
  created_by: bigint;
}

interface updateSubCategoryBody {
  name: string;
  category_id: number;
  budget: number;
  updated_by: bigint;
  sub_category_id: number;
}

export { createSubCategoryBody, updateSubCategoryBody };
