interface createGstBody {
  rate: number;
  cgst_rate: number;
  igst_rate: number;
  sgst_rate: number;
  created_by: bigint;
}

interface updateGstBody {
  rate: number;
  cgst_rate: number;
  igst_rate: number;
  updated_by: bigint;
  sgst_rate: number;
  gst_id: number;
}

export { createGstBody, updateGstBody };
