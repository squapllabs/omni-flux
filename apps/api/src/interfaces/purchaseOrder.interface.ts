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
  purchase_order_item: Array<PurchaseOrderItem>;
}

interface PurchaseOrderItem {
  purchase_order_item_id: number;
  purchase_order_id: number;
  item_id: number;
  order_quantity: number;
  unit_price: number;
  created_by: number;
  updated_by: number;
}

export { purchaseOrderBody };
