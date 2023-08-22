interface createSubCategoryBody {
  name: string;
  category_id: number;
  budget: number;
  created_by: bigint;
  description: string;
}

interface updateSubCategoryBody {
  name: string;
  category_id: number;
  budget: number;
  updated_by: bigint;
  sub_category_id: number;
  description: string;
}

export { createSubCategoryBody, updateSubCategoryBody };
