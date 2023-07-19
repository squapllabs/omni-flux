interface createProjectExpenseBody {
  project_id: number | null;
  description: string | null;
  amount: number | null;
  date: Date | null;
  document_url: string | null;
  created_by: bigint | null;
}

interface updateProjectExpenseBody {
  project_expense_id: number;
  project_id: number | null;
  description: string | null;
  amount: number | null;
  date: Date | null;
  document_url: string | null;
  updated_by: bigint | null;
}

export { createProjectExpenseBody, updateProjectExpenseBody };
