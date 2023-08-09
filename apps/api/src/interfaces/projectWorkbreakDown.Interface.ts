interface createProjectWorkbreakDownBody {
  project_workbreak_down_name: string;
  project_workbreak_down_description: string;
  project_workbreak_down_code: string;
  parent_project_workbreak_down_id: number;
  rate: number;
  uom_id: number;
  project_workbreak_down_type: string;
  project_id: number;
  site_id: number;
  created_by: number;
}

interface updateProjectWorkbreakDownBody {
  project_workbreak_down_id: number;
  project_workbreak_down_name: string;
  project_workbreak_down_description: string;
  project_workbreak_down_code: string;
  parent_project_workbreak_down_id: number;
  rate: number;
  uom_id: number;
  project_workbreak_down_type: string;
  project_id: number;
  site_id: number;
  updated_by: number;
}

export { createProjectWorkbreakDownBody, updateProjectWorkbreakDownBody };
