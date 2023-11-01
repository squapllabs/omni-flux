import React from 'react';
import { format } from 'date-fns';
import ExcelJS from 'exceljs';

const PurchaseOrder = async (datas: any) => {
    // const { excelData: datas } = props;



    console.log("datas", datas);



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
    }

    // Define column headers
    const headers = [
        'Purchase Centre Name',
        'Project Name',
        'Order Type',
        'Order No.',
        'Order Date',
        'Release Date',
        'Vendor Code',
        'Vendor Name',
        'Order Currency',
        'Exchange Rate',
        'Total Amount',
        'PO Description',
        'Purchase Type',
        'Ref Type & No.',
        'Ref No.',
        'Prepared By',
        'Approved By',
        'Closed On',
        'Closed By',
    ];
    const itemHeader = [
        '',
        'Sl No.',
        'Code',
        'Name',
        'Attributes',
        'Specification',
        'UOM',
        'Quantity',
        'Nos',
        'Duration',
        'Duration UOM',
        'Charge UOM',
        'Rate',
        'Others',
        'Net Amount',
    ];




    // Add header row with styles
    let headerAdded = false; // Track if the header row has been added
    if (datas?.length > 1) {
        datas?.forEach((data: any, index: number) => {
            if (!headerAdded) {
                worksheet.addRow(headers).eachCell((cell) => {
                    cell.style = headerStyle;
                });
                headerAdded = true;
            }

            // if(!data?.purchase_request_data){
            // }

            const headerData = data?.purchase_request_data ? [
                'Head Office',
                data?.purchase_request_data?.project_data?.project_name,
                'Purchase Order',
                data?.order_id,
                format(new Date(data?.order_date), 'MM/dd/yyyy'),
                format(new Date(data?.purchase_request_data?.indent_request_data?.expected_delivery_date), 'MM/dd/yyyy'),
                'N/A',
                data?.purchase_request_data?.selected_vendor_data?.vendor_name,
                'IND',
                'N/A',
                data?.purchase_request_data?.total_cost,
                'N/A',
                'N/A',
                'N/A',
                'N/A',
                data?.purchase_request_data?.requester_user_data?.first_name + " " + data?.purchase_request_data?.requester_user_data?.last_name,
                'N/A',
                'N/A',
                'N/A'
            ] : [
                'Head Office',
                data?.purchase_request_data?.project_data?.project_name,
                'Purchase Order',
                data?.order_id,
                format(new Date(data?.order_date), 'MM/dd/yyyy'),
                format(new Date(data?.purchase_request_data?.indent_request_data?.expected_delivery_date), 'MM/dd/yyyy'),
                'N/A',
                data?.purchase_request_data?.selected_vendor_data?.vendor_name,
                'IND',
                'N/A',
                data?.purchase_request_data?.total_cost,
                'N/A',
                'N/A',
                'N/A',
                'N/A',
                data?.purchase_request_data?.requester_user_data?.first_name + " " + data?.purchase_request_data?.requester_user_data?.last_name,
                'N/A',
                'N/A',
                'N/A'];

            worksheet.addRow(headerData).eachCell((cell) => {
                cell.style = borderStyle;
            });

            let itemsData = [];
            itemsData = data?.purchase_request_data?.purchase_request_quotation_details.map((item: any, rowIndex: number) => {
                return {
                    '': '',
                    'Sl No.': (rowIndex + 1).toString(),
                    'Code': " N/A",
                    'Name': item?.item_data?.item_name,
                    'Attributes': 'N/A',
                    'Specification': 'N/A',
                    'UOM': item?.item_data?.uom?.name,
                    'Quantity': item?.purchase_requested_quantity.toString(),
                    'Nos': 'N/A',
                    'Duration': 'N/A',
                    'Duration UOM': 'N/A',
                    'Charge UOM': 'N/A',
                    'Rate': item?.item_data?.rate,
                    'Others': item?.purchase_requested_quantity * item?.item_data?.rate,
                    'Net Amount': "N/A",
                };
            });


            if (itemsData && itemsData.length > 0) {
                worksheet.addRow(itemHeader).eachCell((cell, colNumber) => {
                    if (colNumber > 1) {
                        cell.style = headerStyle;
                    }
                });
            }

            itemsData?.forEach((item: any) => {
                const rowData = Object.values(item);
                worksheet.addRow(rowData).eachCell((cell, colNumber) => {
                    if (colNumber > 1) { // Skip the first column (A3)
                        cell.style = borderStyle;
                    }
                });
            });
        });
    }

    else {
        const headerData = [
            'Head Office',
            datas?.purchase_request_data?.project_data?.project_name,
            'Purchase Order',
            datas?.order_id,
            format(new Date(datas?.order_date), 'MM/dd/yyyy'),
            format(new Date(datas?.purchase_request_data?.indent_request_data?.expected_delivery_date), 'MM/dd/yyyy'),
            'N/A',
            datas?.purchase_request_data?.selected_vendor_data?.vendor_name,
            'IND',
            'N/A',
            datas?.purchase_request_data?.total_cost,
            'N/A',
            'N/A',
            'N/A',
            'N/A',
            datas?.purchase_request_data?.requester_user_data?.first_name + " " + datas?.purchase_request_data?.requester_user_data?.last_name,
            'N/A',
            'N/A',
            'N/A'
        ];

        let itemsData = [];
        itemsData = datas?.purchase_request_data?.purchase_request_quotation_details.map((item: any, index: number) => {
            return {
                '': '',
                'Sl No.': (index + 1).toString(),
                'Code': " N/A",
                'Name': item?.item_data?.item_name,
                'Attributes': 'N/A',
                'Specification': 'N/A',
                'UOM': item?.item_data?.uom?.name,
                'Quantity': item?.purchase_requested_quantity?.toString(),
                'Nos': 'N/A',
                'Duration': 'N/A',
                'Duration UOM': 'N/A',
                'Charge UOM': 'N/A',
                'Rate': item?.item_data?.rate,
                'Others': item?.purchase_requested_quantity * item?.item_data?.rate,
                'Net Amount': "N/A",
            };
        });
        // Add header row with styles
        worksheet.addRow(headers).eachCell((cell) => {
            cell.style = headerStyle;
        });

        worksheet.addRow(headerData).eachCell((cell) => {
            cell.style = borderStyle;
        });
        worksheet.addRow(itemHeader).eachCell((cell, colNumber) => {
            if (colNumber > 1) { // Skip the first column (A3)
                cell.style = headerStyle;
            }
        });
        itemsData?.forEach((item) => {
            const rowData = Object.values(item);
            worksheet.addRow(rowData).eachCell((cell, colNumber) => {
                if (colNumber > 1) { // Skip the first column (A3)
                    cell.style = borderStyle;
                }
            });
        });
    }

    // props.setDownloading(false)



    // const worksheet = workbook.addWorksheet('Sheet1');
    // Generate a buffer containing the Excel file
    const buffer = await workbook.xlsx.writeBuffer();

    // Create a Blob from the buffer
    const blob = new Blob([buffer], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });

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

export default PurchaseOrder;
