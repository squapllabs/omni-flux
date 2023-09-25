interface expenseBody {
  expense_id: number;
  expense_code: string;
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
  bill_details: JSON;
  created_by: number;
  updated_by: number;
  status: string;
  comments: string;
  progressed_date: Date;
  progressed_by: number;
  expense_details: Array<expenseDetailsBody>;
}

interface expenseDetailsBody {
  expense_details_id: number;
  expense_id: number;
  expense_data_id: number;
  total: number;
  bill_details: JSON;
  created_by: number;
  updated_by: number;
  status: string;
  comments: string;
  progressed_date: Date;
  progressed_by: number;
  is_delete: boolean;
  bill_number: string;
}

export { expenseBody, expenseDetailsBody };
