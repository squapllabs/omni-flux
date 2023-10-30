import React from 'react'
import * as XLSX from 'xlsx';
import { format } from 'date-fns';

const PurchaseOrder = (datas: any) => {

    console.log("data", datas);

    const exportData = () => {
        // Sample data

        let itemsData = [];

        // Mapping item_data to Excel data
        itemsData = datas?.purchase_request_data?.purchase_request_quotation_details.map((item: any, index: number) => {
            return {
                '': '',
                'Sl No.': (index + 1).toString(),
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

        console.log("itemsData", itemsData);

        const data = [
            [
                // { text: 'Purchase Centre Name', fill: { type: 'pattern', pattern: 'solid', bgColor: { argb: 'FFFF00' } }, font: { color: { argb: '000000' } } },
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
            ],
            [
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
            ],
            [
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
            ],

        ];

        data.push(
            // ... (previous rows)
            ...itemsData.map(item => Object.values(item))
        );

        // Create a new workbook
        const wb = XLSX.utils.book_new();
        const ws = XLSX.utils.aoa_to_sheet(data);

        // Define styles for the header row
        const headerStyle = {
            font: { color: { rgb: '000000' } }, // Black font color
            fill: { fgColor: { rgb: 'FFFF00' } }, // Yellow background color
        };

        // Apply styles to specific cells
        for (let col = 0; col < data[0].length; col++) {
            const cellAddress = XLSX.utils.encode_cell({ r: 0, c: col }); // Address of header row
            ws[cellAddress].s = headerStyle;
        }

        // Add the worksheet to the workbook
        XLSX.utils.book_append_sheet(wb, ws, 'Sheet1');

        // Generate a file and save it
        XLSX.writeFile(wb, 'exported_data.xlsx');
    };



    exportData();


}

export default PurchaseOrder