interface grnBody {
  grn_id: number;
  project_id: number;
  purchase_order_id: number;
  goods_received_by: number;
  goods_received_date: Date;
  invoice_id: string;
  invoice_amount: number;
  notes: string;
  bill_details: JSON;
  grn_status: string;
  created_by: number;
  grn_details: Array<grnDetailsBody>;
  site_id: number;
  purchase_order_type: string;
}

interface grnDetailsBody {
  grn_details_id: number;
  grn_id: number;
  item_id: number;
  received_quantity: number;
  accepted_quantity: number;
  rejected_quantity: number;
  notes: string;
  created_by: number;
  purchase_order_item_id: number;
  order_quantity: number;
  unit_price: number;
  previously_received_quantity: number;
  currently_received_quantity: number;
  /* Expense Related Data */
  expense_data_id: number;
  bill_number: string;
  bill_details: JSON;
  total: number;
  description: string;
  quantity: number;
  unit_value: number;
  bill_type: string;
  is_delete: boolean;
  status: string;
}

export { grnBody, grnDetailsBody };
