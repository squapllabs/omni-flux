interface createSiteBody {
  site_name: string;
  location: string;
  user_id: number;
  created_date: Date;
  updated_date: Date;
  created_by: bigint;
}

interface updateSiteBody {
  site_id: number;
  site_name: string;
  location: string;
  user_id: number;
  created_date: Date;
  updated_date: Date;
  updated_by: bigint;
}

export { createSiteBody, updateSiteBody };
