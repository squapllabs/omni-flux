interface stockOutwardBody {
  stock_outward_id: number;
  outward_id: string;
  project_id: number;
  site_id: number;
  site_engineer_id: number;
  item_count: number;
  stock_outward_date: Date;
  created_by: number;
  updated_by: number;
  stock_outward_details: Array<stockOutwardDetails>;
}

interface stockOutwardDetails {
  stock_outward_details_id: number;
  stock_outward_id: number;
  item_id: number;
  outward_quantity: number;
  uom_id: number;
  created_by: number;
  updated_by: number;
  is_delete: boolean;
}

export { stockOutwardBody };
