import { format } from 'date-fns';
import ExcelJS from 'exceljs';

const PurchaseRegisterItem = async (data: any) => {
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
    'Purchase Type',
    'Project Code',
    'Project Name',
    'Order Number',
    'Order Date',
    'Vendor Name',
    'Total Amount',
    'Requested By',
  ];

  const itemHeader = [
    '',
    'S No',
    'Item Name',
    'UOM',
    'Quantity',
    'Rate',
    'Total Amount',
  ];

  let headerData: any = [];
  let itemsData: any = [];
  let headerAdded = false; // Track if the header row has been added
  if (data?.length > 1) {
    data?.forEach((itemdata: any, index: number) => {
      // console.log("itemdata 000009", itemdata);

      if (!headerAdded) {
        worksheet.addRow(headers).eachCell((cell) => {
          cell.style = headerStyle;
        });
        headerAdded = true;
      }

      // if (itemdata) {
      // headerData = itemdata?.map((item: any, rowIndex: number) => {
      if (itemdata?.purchase_order_type === 'Head Office') {
        // return {
        headerData = [
          (index + 1).toString(),
          itemdata?.purchase_order_type,
          itemdata?.purchase_request_data?.project_data?.code,
          itemdata?.purchase_request_data?.project_data?.project_name,
          itemdata?.order_id,
          format(new Date(itemdata?.order_date), 'MM/dd/yyyy'),
          itemdata?.purchase_request_data?.selected_vendor_data?.vendor_name,
          itemdata?.purchase_request_data?.total_cost,
          itemdata?.purchase_request_data?.requester_user_data?.first_name +
            ' ' +
            itemdata?.purchase_request_data?.requester_user_data?.last_name,
        ];
      } else {
        headerData = [
          (index + 1).toString(),
          itemdata?.purchase_order_type,
          itemdata?.indent_request_data?.project_data?.code,
          itemdata?.indent_request_data?.project_data?.project_name,
          itemdata?.order_id,
          format(new Date(itemdata?.order_date), 'MM/dd/yyyy'),
          itemdata?.purchase_request_data?.selected_vendor_data?.vendor_name ||
            'N/A',
          itemdata?.indent_request_data?.total_cost,
          itemdata?.indent_request_data?.requester_user_data?.first_name +
            ' ' +
            itemdata?.indent_request_data?.requester_user_data?.last_name,
        ];
      }
      if (itemdata?.purchase_order_type === 'Head Office') {
        itemsData =
          itemdata?.purchase_request_data?.purchase_request_quotation_details?.map(
            (data: any, rowIndex: number) => {
              return {
                '': '',
                'Sl No.': (rowIndex + 1).toString(),
                'Item Name': data?.item_data?.item_name,
                UOM: data?.item_data?.uom?.name,
                Quantity: data?.purchase_requested_quantity,
                Rate: data?.item_data?.rate,
                'Total Amount':
                  data?.purchase_requested_quantity * data?.item_data?.rate,
              };
            }
          );
      } else {
        itemsData = itemdata?.indent_request_data?.indent_request_details?.map(
          (data: any, rowIndex: number) => {
            return {
              '': '',
              'Sl No.': (rowIndex + 1).toString(),
              'Item Name': data?.bom_detail_data?.item_data?.item_name,
              UOM: data?.bom_detail_data?.item_data?.uom?.name,
              Quantity: data?.indent_requested_quantity,
              Rate: data?.bom_detail_data?.item_data?.rate,
              'Total Amount':
                data?.indent_requested_quantity *
                data?.bom_detail_data?.item_data?.rate,
            };
          }
        );
      }

      worksheet.addRow(headerData).eachCell((cell) => {
        cell.style = borderStyle;
      });

      if (itemsData && itemsData?.length > 0) {
        worksheet.addRow(itemHeader).eachCell((cell, colNumber) => {
          if (colNumber > 1) {
            cell.style = headerStyle;
          }
        });
      }

      itemsData?.forEach((item: any) => {
        const rowData = Object.values(item);
        worksheet.addRow(rowData).eachCell((cell, colNumber) => {
          if (colNumber > 1) {
            // Skip the first column (A3)
            cell.style = borderStyle;
          }
        });
      });
    });
  } else {
    worksheet.addRow(headers).eachCell((cell) => {
      cell.style = headerStyle;
    });

    if (data) {
      headerData = data?.map((item: any, rowIndex: number) => {
        if (item?.purchase_order_type === 'Head Office') {
          return {
            'Sl No.': (rowIndex + 1).toString(),
            'Purchase Type': item?.purchase_order_type,
            'Project Code': item?.purchase_request_data?.project_data?.code,
            'Project Name':
              item?.purchase_request_data?.project_data?.project_name,
            'Order Number': item?.order_id,
            'Order Date': format(new Date(item?.order_date), 'MM/dd/yyyy'),
            'Vendor Name':
              item?.purchase_request_data?.selected_vendor_data?.vendor_name,
            'Total Amount': item?.purchase_request_data?.total_cost,
            'Requested By':
              item?.purchase_request_data?.requester_user_data?.first_name +
              ' ' +
              item?.purchase_request_data?.requester_user_data?.last_name,
          };
        } else {
          return {
            'Sl No.': (rowIndex + 1).toString(),
            'Purchase Type': item?.purchase_order_type,
            'Project Code': item?.indent_request_data?.project_data?.code,
            'Project Name':
              item?.indent_request_data?.project_data?.project_name,
            'Order Number': item?.order_id,
            'Order Date': format(new Date(item?.order_date), 'MM/dd/yyyy'),
            'Vendor Name':
              item?.purchase_request_data?.selected_vendor_data?.vendor_name ||
              'N/A',
            'Total Amount': item?.indent_request_data?.total_cost,
            'Requested By':
              item?.indent_request_data?.requester_user_data?.first_name +
              ' ' +
              item?.indent_request_data?.requester_user_data?.last_name,
          };
        }
      });
      data?.map((item: any, rowIndex: number) => {
        // console.log("it", item);
        if (item?.purchase_order_type === 'Head Office') {
          itemsData =
            item?.purchase_request_data?.purchase_request_quotation_details?.map(
              (data: any, rowIndex: number) => {
                // console.log("it33", data);

                return {
                  '': '',
                  'Sl No.': (rowIndex + 1).toString(),
                  'Item Name': data?.item_data?.item_name,
                  UOM: data?.item_data?.uom?.name,
                  Quantity: data?.purchase_requested_quantity,
                  Rate: data?.item_data?.rate,
                  'Total Amount':
                    data?.purchase_requested_quantity * data?.item_data?.rate,
                };
              }
            );
        } else {
          itemsData = item?.indent_request_data?.indent_request_details?.map(
            (data: any, rowIndex: number) => {
              return {
                '': '',
                'Sl No.': (rowIndex + 1).toString(),
                'Item Name': data?.bom_detail_data?.item_data?.item_name,
                UOM: data?.bom_detail_data?.item_data?.uom?.name,
                Quantity: data?.indent_requested_quantity,
                Rate: data?.bom_detail_data?.item_data?.rate,
                'Total Amount':
                  data?.indent_requested_quantity *
                  data?.bom_detail_data?.item_data?.rate,
              };
            }
          );
        }
      });
    }
    headerData?.forEach((item: any) => {
      const rowData = Object.values(item);
      worksheet.addRow(rowData).eachCell((cell, colNumber) => {
        if (colNumber > 1) {
          // Skip the first column (A3)
          cell.style = borderStyle;
        }
      });
    });

    if (itemsData && itemsData?.length > 0) {
      worksheet.addRow(itemHeader).eachCell((cell, colNumber) => {
        if (colNumber > 1) {
          cell.style = headerStyle;
        }
      });
    }

    itemsData?.forEach((item: any) => {
      const rowData = Object.values(item);
      worksheet.addRow(rowData).eachCell((cell, colNumber) => {
        if (colNumber > 1) {
          // Skip the first column (A3)
          cell.style = borderStyle;
        }
      });
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
  a.download = 'purchase_register.xlsx';
  document.body.appendChild(a);
  a.click();
  window.URL.revokeObjectURL(url);
  document.body.removeChild(a);
};

export default PurchaseRegisterItem;
