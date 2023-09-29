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
  purchase_order_documents: Array<PurchaseOrderDocuments>;
  order_id: string;
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

interface PurchaseOrderDocuments {
  is_delete: boolean;
  path: string;
  index: number;
}

export { purchaseOrderBody };
