interface bomConfigurationBody {
  bom_configuration_id: number;
  bom_name: string;
  bom_description: string;
  bom_type_id: number;
  project_id: number;
  budget: number;
  is_delete: boolean;
  created_date: Date;
  updated_date: Date;
  created_by: number;
  updated_by: number;
}
export { bomConfigurationBody };
