import { format } from 'date-fns';
import ExcelJS from 'exceljs';

const AccountsProjectSummery = async (data: any) => {

  const workbook = new ExcelJS.Workbook();
  const worksheet = workbook.addWorksheet('Sheet1');

  const headerStyle = {
    font: { color: { argb: '000000' } }, // Black font color
    fill: { type: 'pattern', pattern: 'solid', fgColor: { argb: 'd3d3d3' } },
    border: {
      top: { style: 'thin', color: { argb: '000000' } },
      bottom: { style: 'thin', color: { argb: '000000' } },
      left: { style: 'thin', color: { argb: '000000' } },
      right: { style: 'thin', color: { argb: '000000' } },
    },
  };

  const borderStyle = {
    border: {
      top: { style: 'thin', color: { argb: '000000' } },
      bottom: { style: 'thin', color: { argb: '000000' } },
      left: { style: 'thin', color: { argb: '000000' } },
      right: { style: 'thin', color: { argb: '000000' } },
    },
  };

  const headers = [
    'S No',
    'Project Code',
    'Project Name',
    'Invoice Number',
    'Invoice Date',
    'Payment Status',
    'Payment Date',
    'Payment Mode',
    'Total Amount',
    'Requested By',
    'Paid By',
  ];

  let headerData: any = [];
  let headerAdded = false; // Track if the header row has been added

  if (data?.length > 1) {
    data?.forEach((itemdata: any, index: number) => {
      if (!headerAdded) {
        worksheet.addRow(headers).eachCell((cell) => {
          cell.style = headerStyle;
        });
        headerAdded = true;
      }
      headerData = [
        index + 1,
        itemdata?.purchase_order_data?.purchase_request_data?.project_data
          ?.code || 'N/A',
        itemdata?.purchase_order_data?.purchase_request_data?.project_data
          ?.project_name || 'N/A',
        itemdata?.invoice_number || 'N/A',
        format(new Date(itemdata?.invoice_date), 'MM/dd/yyyy') || 'N/A',
        itemdata?.status || 'N/A',
        format(new Date(itemdata?.paid_date), 'MM/dd/yyyy') || 'N/A',
        itemdata?.payment_mode || 'N/A',
        itemdata?.total_amount || 'N/A',
        itemdata?.requested_by_data?.first_name +
        ' ' +
        itemdata?.requested_by_data?.last_name || 'N/A',
        itemdata?.paid_by_data === null
          ? 'N/A'
          : itemdata?.paid_by_data?.first_name +
          ' ' +
          itemdata?.paid_by_data?.last_name,
      ];

      worksheet.addRow(headerData).eachCell((cell) => {
        cell.style = borderStyle;
      });
    });
  } else {
    worksheet.addRow(headers).eachCell((cell) => {
      cell.style = headerStyle;
    });

    data?.map?.((data: any, index: number) => {
      headerData = [
        index + 1,
        data?.purchase_order_data?.purchase_request_data?.project_data?.code ||
        'N/A',
        data?.purchase_order_data?.purchase_request_data?.project_data
          ?.project_name || 'N/A',
        data?.invoice_number || 'N/A',
        data?.invoice_date || 'N/A',
        data?.status || 'N/A',
        data?.paid_date || 'N/A',
        data?.payment_mode || 'N/A',
        data?.total_amount || 'N/A',
        data?.requested_by_data?.first_name +
        ' ' +
        data?.requested_by_data?.last_name || 'N/A',
        data?.paid_by || 'N/A',
      ];
    });

    worksheet.addRow(headerData).eachCell((cell) => {
      cell.style = borderStyle;
    });
  }

  worksheet.columns.forEach((column) => {
    let maxLength = 0;
    column.eachCell({ includeEmpty: true }, (cell) => {
      const length = cell.value ? cell.value.toString().length : 10;
      if (length > maxLength) {
        maxLength = length;
      }
    });
    column.width = maxLength < 10 ? 10 : maxLength + 2; // Add some padding
  });

  // Generate a buffer containing the Excel file
  const buffer = await workbook.xlsx.writeBuffer();

  // Create a Blob from the buffer
  const blob = new Blob([buffer], {
    type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
  });

  // Create a download link and trigger the download
  const url = window.URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = 'invoice.xlsx';
  document.body.appendChild(a);
  a.click();
  window.URL.revokeObjectURL(url);
  document.body.removeChild(a);
};

export default AccountsProjectSummery;
