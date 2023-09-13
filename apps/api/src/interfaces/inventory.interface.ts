interface inventoryBody {
  inventory_id: number;
  item_id: number;
  item_name: string;
  item_category: string;
  rate: number;
  available_quantity: number;
  store_id: number;
  project_id: number;
  created_by: number;
  updated_by: number;
}

export { inventoryBody };
