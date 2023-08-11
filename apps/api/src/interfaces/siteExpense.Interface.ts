interface createSiteExpenseBody {
  site_id: number;
  project_id: number;
  employee_name: string;
  employee_id: string;
  employee_phone: string;
  purpose: string;
  department: string;
  designation: string;
  start_date: Date;
  end_date: Date;
  created_by: bigint;
  /* Site Expense Details Data */
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
}

interface updateSiteExpenseBody {
  site_id: number;
  project_id: number;
  employee_name: string;
  employee_id: string;
  employee_phone: string;
  purpose: string;
  department: string;
  designation: string;
  start_date: Date;
  end_date: Date;
  updated_by: bigint;
  site_expense_id: number;
  /* Site Expense Details Data */
  site_expense_details: Array<siteExpenseDetails>;
}

export { createSiteExpenseBody, updateSiteExpenseBody, siteExpenseDetails };
