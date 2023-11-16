import ExcelJS from 'exceljs';

const ProjectInwardReport = async (datas: any) => {
  console.log('ProjectInwardReport', datas);

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

  // Define column headers
  const headers = [
    'Item Name / description',
    'UOM',
    datas != null ? datas[0]?.project_data?.project_name : '',
  ];
  console.log('headers', headers);

  // worksheet.mergeCells('B1:C1');
  // Add header row with styles
  let headerAdded = false; // Track if the header row has been added
  if (datas != null) {
    datas?.forEach((data: any, index: number) => {
      if (!headerAdded) {
        worksheet.addRow(headers).eachCell((cell) => {
          cell.style = headerStyle;
        });
        const subHeaderCell = worksheet.getCell('C2');
        subHeaderCell.value = 'Quantity';
        const subHeaderCell2 = worksheet.getCell('D2');
        subHeaderCell2.value = 'Value';
        subHeaderCell.font = { color: { argb: '000000' } };
        subHeaderCell.fill = {
          type: 'pattern',
          pattern: 'solid',
          fgColor: { argb: 'd3d3d3' }, // Yellow background color
        };
        subHeaderCell2.font = { color: { argb: '000000' } };
        subHeaderCell2.fill = {
          type: 'pattern',
          pattern: 'solid',
          fgColor: { argb: 'd3d3d3' }, // Yellow background color
        };
        subHeaderCell2.border = {
          top: { style: 'thin', color: { argb: '000000' } },
          bottom: { style: 'thin', color: { argb: '000000' } },
          left: { style: 'thin', color: { argb: '000000' } },
          right: { style: 'thin', color: { argb: '000000' } },
        };
        subHeaderCell.border = {
          top: { style: 'thin', color: { argb: '000000' } },
          bottom: { style: 'thin', color: { argb: '000000' } },
          left: { style: 'thin', color: { argb: '000000' } },
          right: { style: 'thin', color: { argb: '000000' } },
        };
        worksheet.mergeCells('A1:A2');
        worksheet.mergeCells('B1:B2');
        worksheet.mergeCells('C1:D1');
        headerAdded = true;
      }

      const headerData = [
        data?.item_data?.item_name,
        data?.item_data?.uom?.name,
        data?.available_quantity,
        data?.total_cost,
      ];
      console.log('headerData', headerData);

      worksheet.addRow(headerData).eachCell((cell) => {
        cell.style = borderStyle;
      });
    });
    const lastRowWithData = worksheet.actualRowCount;
    console.log(`The last row with data is: ${lastRowWithData}`);
    const finalBody = worksheet.getCell(`B${lastRowWithData + 1}`);
    finalBody.value = 'TOTAL';
    const sumofQuantity = datas?.reduce(
      (accumulator: any, currentItem: any) => {
        return accumulator + currentItem.available_quantity;
      },
      0
    );
    const sumofCost = datas?.reduce((accumulator: any, currentItem: any) => {
      return accumulator + currentItem.total_cost;
    }, 0);
    console.log('sumofQuantity', sumofQuantity);
    const finalBody2 = worksheet.getCell(`C${lastRowWithData + 1}`);
    finalBody2.value = sumofQuantity;
    const finalBody3 = worksheet.getCell(`D${lastRowWithData + 1}`);
    finalBody3.value = sumofCost;
    worksheet.getCell(`B${lastRowWithData + 1}`).style = headerStyle;
    worksheet.getCell(`C${lastRowWithData + 1}`).style = headerStyle;
    worksheet.getCell(`D${lastRowWithData + 1}`).style = headerStyle;
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
  } else {
    const headers = [
      'Item Name / description',
      'UOM',
      datas != null ? datas[0]?.project_data?.project_name : '',
    ];
    worksheet.addRow(headers).eachCell((cell) => {
      cell.style = headerStyle;
    });
    console.log('empty');
  }

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
  a.download = 'purchase_order.xlsx';
  document.body.appendChild(a);
  a.click();
  window.URL.revokeObjectURL(url);
  document.body.removeChild(a);
};

export default ProjectInwardReport;
