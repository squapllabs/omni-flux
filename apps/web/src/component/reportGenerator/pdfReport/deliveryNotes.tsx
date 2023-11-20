import jsPDF from 'jspdf';
import 'jspdf-autotable';
import { format } from 'date-fns';
import { companyDetails } from '../../../helper/commonContent/companyDetails';

const DeliveryNotes = (data: any, GRNData: any) => {

  const itemsData = GRNData?.tableData?.map((item: any) => ({
    itemName: item?.item_name,
    allocatedQuantity: item?.order_quantity,
    remainingQuantity: item?.inward_remaining_quantity,
    previouslyReceivedQuantity: item?.previously_received_quantity,
    currentlyReceivedQuantity: item?.currently_received_quantity,
  }));


  const purchaseOrder =
    data?.purchaseOrder === 'Head Office'
      ? {
        companyName: companyDetails.companyName,
        companyAddress: companyDetails.companyAddress,
        Line1: companyDetails.Line1,
        phoneNumber: companyDetails.phoneNumber,
        emailAddress: companyDetails.emailAddress,
        websiteURL: companyDetails.websiteURL,
        orderNumber: data?.order_id,
        orderType: data?.purchase_order_type,
        orderDate: format(new Date(data?.order_date), 'MMM dd, yyyy'),
        invoiceNumber: GRNData?.invoiceNumber,
        notes: GRNData?.notes,
        vendorName:
          data?.purchase_request_data?.selected_vendor_data?.vendor_name ||
          'N/A',
        vendorAddress: `${data?.purchase_request_data?.selected_vendor_data?.address
            ?.street || ''
          } ${data?.purchase_request_data?.selected_vendor_data?.address?.city ||
          ''
          } ${data?.purchase_request_data?.selected_vendor_data?.address?.state ||
          ''
          } ${data?.purchase_request_data?.selected_vendor_data?.address
            ?.country || ''
          } ${data?.purchase_request_data?.selected_vendor_data?.address
            ?.pin_code || ''
          }`,
        vendorContact:
          data?.purchase_request_data?.selected_vendor_data
            ?.contact_phone_no || 'N/A',
        siteName: data?.purchase_request_data?.site_data?.name || 'N/A',
        siteAddress: `${data?.purchase_request_data?.site_data?.address?.street || ''
          } ${data?.purchase_request_data?.site_data?.address?.city || ''} ${data?.purchase_request_data?.site_data?.address?.state || ''
          } ${data?.purchase_request_data?.site_data?.address?.country || ''} ${data?.purchase_request_data?.site_data?.address?.pin_code || ''
          }`,
        siteContact:
          data?.purchase_request_data?.site_data?.mobile_number || 'N/A',
      }
      : {
        companyName: companyDetails.companyName,
        companyAddress: companyDetails.companyAddress,
        Line1: companyDetails.Line1,
        phoneNumber: companyDetails.phoneNumber,
        emailAddress: companyDetails.emailAddress,
        websiteURL: companyDetails.websiteURL,
        orderType: data?.purchase_order_type,
        orderNumber: data?.order_id,
        orderDate: format(new Date(data?.order_date), 'MMM dd, yyyy'),
        invoiceNumber: GRNData?.invoiceNumber,
        notes: GRNData?.notes,
        vendorName:
          data?.purchase_request_data?.selected_vendor_data?.vendor_name ||
          'N/A',
        vendorAddress: `${data?.purchase_request_data?.selected_vendor_data?.address
            ?.street || ''
          } ${data?.purchase_request_data?.selected_vendor_data?.address?.city ||
          ''
          } ${data?.purchase_request_data?.selected_vendor_data?.address?.state ||
          ''
          } ${data?.purchase_request_data?.selected_vendor_data?.address
            ?.country || ''
          } ${data?.purchase_request_data?.selected_vendor_data?.address
            ?.pin_code || ''
          }`,
        vendorContact:
          data?.purchase_request_data?.selected_vendor_data
            ?.contact_phone_no || 'N/A',
        siteName: data?.indent_request_data?.site_data?.name || 'N/A',
        siteAddress: `${data?.indent_request_data?.site_data?.address?.street || ''
          } ${data?.indent_request_data?.site_data?.address?.city || ''} ${data?.indent_request_data?.site_data?.address?.state || ''
          } ${data?.indent_request_data?.site_data?.address?.country || ''} ${data?.indent_request_data?.site_data?.address?.pin_code || ''
          }`,
        siteContact:
          data?.indent_request_data?.site_data?.mobile_number || 'N/A',
      };

  // Create a new jsPDF instance
  const pdf = new jsPDF();
  pdf.setProperties({
    title: 'purchase order',
  });

  // Add content to the PDF
  const yStart = 15;
  const yOffset = 10;

  pdf.setFont('custom');

  const imageUrl = '/Ecologo-03.png'; // Replace with your image URL
  pdf.addImage(imageUrl, 'JPEG', 10, 5, 40, 10);
  // Title
  pdf.setFontSize(10);
  pdf.setFont('Newsreader', 'bold');
  pdf.text('DELIVERY NOTES', 150, 12);
  pdf.setLineWidth(0.1); // Line width in units (you can adjust this)
  pdf.setDrawColor(200, 200, 200); // Line color (RGB)
  pdf.line(10, 18, 200, 18);
  pdf.setFontSize(10);
  pdf.setFont('Newsreader', 'normal');
  pdf.text(`${purchaseOrder.companyAddress}`, 14, 25);
  pdf.text(`${purchaseOrder.Line1}`, 14, 30);
  pdf.text(`${purchaseOrder.phoneNumber}`, 14, 35);
  pdf.text(`${purchaseOrder.emailAddress}`, 14, 40);
  pdf.text(`${purchaseOrder.websiteURL}`, 14, 45);
  pdf.setFontSize(10);
  pdf.text(`Invoice Number: ${purchaseOrder.invoiceNumber}`, 140, 25);
  pdf.text(`Order Number: ${purchaseOrder.orderNumber}`, 140, 30);
  pdf.text(`Order Date: ${purchaseOrder.orderDate}`, 140, 35);
  pdf.text(`Order Type: ${purchaseOrder.orderType}`, 140, 40);

  const headerStyles = {
    fillColor: [240, 240, 240],
    textColor: [0],
    fontFamily: 'Newsreader',
    fontStyle: 'bold',
  };

  const vendorDetailsTable = {
    head: [['Vendor Details']],
    body: [
      [`Vendor Name: ${purchaseOrder.vendorName}`],
      [`Vendor Address: ${purchaseOrder.vendorAddress}`],
      [`Contact Number: ${purchaseOrder.vendorContact}`],
    ],
    startX: 10,
    startY: 50, // Adjust the Y position as needed
    headStyles: {
      fillColor: headerStyles.fillColor,
      textColor: headerStyles.textColor,
      fontStyle: headerStyles.fontStyle,
      fontSize: 10, // Adjust the font size as needed
      font: 'Newsreader', // Set the font family
      halign: 'left',
    },
    alternateRowStyles: { fillColor: [255, 255, 255] },
    bodyStyles: {
      fontSize: 10, // Adjust the font size for the body
      font: 'Newsreader', // Set the font family for the body
      cellPadding: { top: 1, right: 5, bottom: 1, left: 2 }, // Adjust cell padding
      textColor: [0, 0, 0], // Set text color for the body
      rowPageBreak: 'avoid', // Avoid row page breaks
    },
    margin: { top: 10, left: 13 },
  };
  pdf.autoTable(vendorDetailsTable);

  // Create a table for Site / Shipping Details
  const siteDetailsTable = {
    head: [['Site / Shipping Details']],
    body: [
      [`Site Name: ${purchaseOrder.siteName}`],
      [`Site Address: ${purchaseOrder.siteAddress}`],
      [`Contact Number: ${purchaseOrder.siteContact}`],
    ],
    startY: 85, // Adjust the Y position as needed
    headStyles: {
      fillColor: headerStyles.fillColor,
      textColor: headerStyles.textColor,
      fontStyle: headerStyles.fontStyle,
      fontSize: 10, // Adjust the font size as needed
      font: 'Newsreader', // Set the font family
      halign: 'left',
    },
    alternateRowStyles: { fillColor: [255, 255, 255] },
    bodyStyles: {
      fontSize: 10, // Adjust the font size for the body
      font: 'Newsreader', // Set the font family for the body
      cellPadding: { top: 1, right: 5, bottom: 1, left: 2 }, // Adjust cell padding
      textColor: [0, 0, 0], // Set text color for the body
      rowPageBreak: 'avoid', // Avoid row page breaks
    },
    margin: { top: 10, left: 13 },
  };
  pdf.autoTable(siteDetailsTable);

  // Define item details table headers
  const itemDetailsHeaders = [
    'S.No',
    'Item Name',
    'Allocated \n Quantity',
    'Previously \n Received',
    'To Be \n Received',
    'Currently \n Received',
  ];
  // pdf.setFont('courier');
  const itemDetailsRows = itemsData?.map((item: any, index: number) => [
    index + 1,
    item.itemName,
    item.allocatedQuantity?.toString(),
    item.previouslyReceivedQuantity?.toString(),
    item.remainingQuantity?.toLocaleString(),
    item.currentlyReceivedQuantity?.toLocaleString(),
  ]);

  // Create item details table
  const itemDetailsYStart = yStart + yOffset * 10.5;

  pdf.autoTable({
    head: [itemDetailsHeaders],
    body: itemDetailsRows,
    startY: itemDetailsYStart, // Adjust the Y position as needed
    headStyles: {
      fillColor: headerStyles.fillColor,
      textColor: headerStyles.textColor,
      fontStyle: headerStyles.fontStyle,
      fontSize: 10, // Adjust the font size as needed
      font: 'Newsreader', // Set the font family
      halign: 'left',
    },
    alternateRowStyles: { fillColor: [255, 255, 255] },
    bodyStyles: {
      fontSize: 10, // Adjust the font size for the body
      font: 'Newsreader', // Set the font family for the body
      cellPadding: { top: 1, right: 5, bottom: 1, left: 2 }, // Adjust cell padding
      textColor: [0, 0, 0], // Set text color for the body
      rowPageBreak: 'avoid', // Avoid row page breaks
    },
    margin: { top: 10, left: 13 },
  });


  const totalPages = pdf.internal.getNumberOfPages();
  for (let i = 1; i <= totalPages; i++) {
    pdf.setPage(i);
    pdf.setFont('Newsreader');
    pdf.text(
      `Page ${i} of ${totalPages}`,
      185,
      pdf.internal.pageSize.getHeight() - 5
    );
  }

  // Create summary table
  const summaryYStart = pdf.lastAutoTable.finalY;

  pdf.setFont('Newsreader', 'bold');
  pdf.text('Remarks :', 14, summaryYStart + 20);
  pdf.setFont('Newsreader', 'normal');
  pdf.text(`${purchaseOrder?.notes}`, 16, summaryYStart + 25);

  // Save the PDF
  pdf.save(`${purchaseOrder?.orderNumber}_delivery_notes.pdf`);

  // const pdfDataUri = pdf.output("datauristring");
  // const previewWindow = window.open();
  // previewWindow?.document.write(
  //     "<iframe width='100%' height='100%' src='" + pdfDataUri + "'></iframe>"
  // );
};

export default DeliveryNotes;
