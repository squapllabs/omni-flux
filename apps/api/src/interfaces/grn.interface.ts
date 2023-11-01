interface grnBody {
  grn_id: number;
  project_id: number;
  purchase_order_id: number;
  goods_received_by: number;
  goods_received_date: Date;
  invoice_id: string;
  notes: string;
  bill_details: JSON;
  grn_status: string;
  created_by: number;
  grn_details: Array<grnDetailsBody>;
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
}

export { grnBody, grnDetailsBody };
