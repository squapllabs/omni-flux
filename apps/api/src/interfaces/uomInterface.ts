interface createUomBody {
  name: string;
  description: string;
  created_by: bigint;
}

interface updateUomBody {
  name: string;
  description: string;
  updated_by: bigint;
  uom_id: number;
}

export { createUomBody, updateUomBody };
