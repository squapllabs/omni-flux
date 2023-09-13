interface purchaseRequestBody {
  purchase_request_id: number;
  indent_request_id: number;
  requester_user_id: number;
  request_date: Date;
  status: string;
  vendor_selection_method: string;
  project_id: number;
  selected_vendor_id: number;
  total_cost: number;
  is_delete: boolean;
  created_by: number;
  updated_by: number;
}

export { purchaseRequestBody };
