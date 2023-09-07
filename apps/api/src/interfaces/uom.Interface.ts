interface createUomBody {
  name: string;
  description: string;
  created_by: bigint;
  uom_type: string;
}

interface updateUomBody {
  name: string;
  description: string;
  updated_by: bigint;
  uom_id: number;
  uom_type: string;
}

export { createUomBody, updateUomBody };
