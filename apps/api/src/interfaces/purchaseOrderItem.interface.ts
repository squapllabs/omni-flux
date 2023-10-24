interface purchaseOrderItemBody {
  purchase_order_item_id: number;
  purchase_order_id: number;
  item_id: number;
  order_quantity: number;
  inward_quantity: number;
  inward_remaining_quantity: number;
  unit_price: number;
  created_by: number;
  updated_by: number;
}

export { purchaseOrderItemBody };
