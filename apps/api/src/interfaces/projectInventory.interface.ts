interface projectInventoryBody {
  project_inventory_id: number;
  project_id: number;
  item_id: number;
  rate: number;
  available_quantity: number;
  total_cost: number;
  created_by: number;
  updated_by: number;
}

export { projectInventoryBody };
