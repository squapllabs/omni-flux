interface siteExpenseDetailsBody {
  description: string;
  air_transport: number;
  fuel: number;
  labour_advance: number;
  phone_stationary: number;
  food_snacks: number;
  purchase_service: number;
  others: number;
  total: number;
  bill_details: JSON;
  site_expense_details_id: number;
  is_delete: string;
  site_expense_id: number;
  created_by: bigint;
  updated_by: bigint;
  project_id: number;
  site_id: number;
  status: string;
  comments: string;
  progressed_by: number;
  site_expense_details: Array<siteExpenseDetails>;
}

interface siteExpenseDetails {
  description: string;
  air_transport: number;
  fuel: number;
  labour_advance: number;
  phone_stationary: number;
  food_snacks: number;
  purchase_service: number;
  others: number;
  total: number;
  bill_details: JSON;
  site_expense_details_id: number;
  is_delete: string;
  site_expense_id: number;
  status: string;
  comments: string;
  progressed_by: number;
}

export { siteExpenseDetailsBody };
