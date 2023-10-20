interface expenseRecallBody {
  expense_recall_id: number;
  project_id: number;
  site_id: number;
  expense_id: number;
  expense_details_id: number;
  recall_creator_id: number;
  recall_date: Date;
  reason: string;
  is_delete: boolean;
  created_by: number;
  updated_by: number;
}

export { expenseRecallBody };
