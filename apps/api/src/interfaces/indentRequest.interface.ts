interface indentRequestBody {
  indent_request_id: number;
  requester_user_id: number;
  requested_date: Date;
  request_status: string;
  priority: string;
  description: string;
  expected_delivery_date: Date;
  total_cost: number;
  approver_user_id: number;
  approver_status: string;
  approved_date: Date;
  rejected_date: Date;
  approver_comments: string;
  created_by: number;
  updated_by: number;
  indent_request_details: Array<indentRequestDetails>;
  project_id: number;
}

interface indentRequestDetails {
  indent_request_details_id: number;
  indent_request_id: number;
  bom_detail_id: number;
  quantity: number;
  total: number;
  is_delete: boolean;
}

export { indentRequestBody };
