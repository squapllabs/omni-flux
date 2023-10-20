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
  purchase_request_details: Array<purchaseRequestDocuments>;
  purchase_request_documents: JSON;
  vendor_ids: Array<number>;
  site_id: number;
}

interface purchaseRequestDocuments {
  purchase_requested_quantity: number;
  indent_requested_quantity: number;
  indent_request_details_id: number;
  item_id: number;
  rate: number;
  item_name: string;
}

export { purchaseRequestBody };
