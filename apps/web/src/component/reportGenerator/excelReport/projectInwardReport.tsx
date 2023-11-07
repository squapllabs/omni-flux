import React from 'react';
import { format } from 'date-fns';
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
  const headers = ['Item Name / description', 'UOM'];

  // Add header row with styles
  let headerAdded = false; // Track if the header row has been added
  if (datas?.length > 1) {
    datas?.forEach((data: any, index: number) => {
      console.log('itrate---1', data);

      if (!headerAdded) {
        worksheet.addRow(headers).eachCell((cell) => {
          cell.style = headerStyle;
        });
        headerAdded = true;
      }

      const headerData = [
        data?.item_data?.item_name,
        data?.item_data?.uom?.name,
      ];
      console.log('headerData', headerData);

      worksheet.addRow(headerData).eachCell((cell) => {
        cell.style = borderStyle;
      });
    });
  } else {
    const headerData = [''];
    // Add header row with styles
    worksheet.addRow(headers).eachCell((cell) => {
      cell.style = headerStyle;
    });

    worksheet.addRow(headerData).eachCell((cell) => {
      cell.style = borderStyle;
    });
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
