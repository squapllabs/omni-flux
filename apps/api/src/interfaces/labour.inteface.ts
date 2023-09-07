interface labourBody {
  labour_id: number;
  labour_type: string;
  rate: number;
  uom_id: number;
  is_delete: boolean;
  created_by: number;
  updated_by: number;
  created_date: Date;
  updated_date: Date;
}

export { labourBody };
