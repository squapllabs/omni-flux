interface purchaseOrderBody {
  purchase_order_id: number;
  purchase_request_id: number;
  vendor_id: number;
  order_date: Date;
  status: string;
  total_cost: number;
  order_remark: string;
  created_by: number;
  updated_by: number;
}

export { purchaseOrderBody };
