interface createSiteExpenseBody {
  site_id: number;
  description: string;
  amount: number;
  date: Date;
  document_url: string;
  created_by: bigint;
}

interface updateSiteExpenseBody {
  site_id: number;
  description: string;
  amount: number;
  date: Date;
  document_url: string;
  updated_by: bigint;
  site_expense_id: number;
}

export { createSiteExpenseBody, updateSiteExpenseBody };
