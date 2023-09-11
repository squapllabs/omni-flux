interface projectMemberAssociationBody {
  project_member_association_id: number;
  project_id: number;
  user_id: number;
  project_role_id: number;
  access_start_date: Date;
  access_end_date: Date;
  is_delete: boolean;
  created_by: number;
  updated_by: number;
}

export { projectMemberAssociationBody };
