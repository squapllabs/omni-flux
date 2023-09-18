interface purchaseOrderItemBody {
  purchase_order_item_id: number;
  purchase_order_id: number;
  item_id: number;
  order_quantity: number;
  unit_price: number;
  created_by: number;
  updated_by: number;
  purchase_order_item_documents: JSON;
}

export { purchaseOrderItemBody };
