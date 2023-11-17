import jsPDF from 'jspdf';
import 'jspdf-autotable';
import { format } from 'date-fns';
import { companyDetails } from '../../../helper/commonContent/companyDetails'; 

const InvoiceReportGenerator = (data: any) => {

  const purchaseOrder = {
    companyName: companyDetails.companyName,
    companyAddress: companyDetails.companyAddress,
    Line1: companyDetails.Line1,
    phoneNumber: companyDetails.phoneNumber,
    emailAddress: companyDetails.emailAddress,
    websiteURL: companyDetails.websiteURL,
    orderNumber: data?.purchase_order_data?.order_id,
    orderDate: format(
      new Date(data?.purchase_order_data?.order_date),
      'MMM dd, yyyy'
    ),
    vendorName: data?.purchase_order_data?.vendor_data?.vendor_name,
    vendorAddress: data?.purchase_order_data?.vendor_data?.address?.street,
    vendorContact: data?.purchase_order_data?.vendor_data?.contact_phone_no,
    vendorMail: data?.purchase_order_data?.vendor_data?.contact_email,
    siteName: 'Anna Nagar Easte',
    siteAddress: '14/203 , J Block , Anna Nagar Easte',
    siteContact: '9442838007',
    subtotal: data?.purchase_order_data?.purchase_request_data.total_cost,
    poType: data?.purchase_order_data?.purchase_order_type,
  };
  const demoData: any = [];
  const itemsData = data?.purchase_order_data?.grn?.map((items: any) => {
    items?.grn_details.map((subItems: any) => {
      demoData.push(subItems);
    });
  });


  // Create a new jsPDF instance
  const pdf = new jsPDF();


  pdf.setFont('custom');
  // Company Name

  const imageUrl = '/Ecologo-03.png'; // Replace with your image URL
  pdf.addImage(imageUrl, 'JPEG', 10, 5, 40, 10); //text to added in list of particals
  const callImage = '/Calling.png';
  // Title
  pdf.setFont('Newsreader', 'bold');
  pdf.setFontSize(11);
  pdf.text(`${data?.title.toUpperCase()}`, 150, 12);
  pdf.setFontSize(10);
  pdf.setFont('custom', 'bold');
  pdf.setLineWidth(0.1); // Line width in units (you can adjust this)
  pdf.setDrawColor(200, 200, 200); // Line color (RGB)
  pdf.line(10, 18, 200, 18);
  pdf.text('From', 13, 23);
  pdf.setFont('custom', 'normal');
  pdf.text(`${purchaseOrder?.companyName}`, 13, 27);
  pdf.text(`${purchaseOrder?.companyAddress}`, 13, 31);
  pdf.addImage(callImage, 'PNG', 13, 32, 3, 3);
  pdf.text(`${purchaseOrder?.phoneNumber}`, 16, 35);
  pdf.text(`${purchaseOrder?.emailAddress}`, 13, 39);
  pdf.setFont('Newsreader', 'bold');
  pdf.text('PO No      :', 130, 23);
  pdf.text('PO Date   :', 130, 27);
  pdf.text('PO Type   :', 130, 31);
  pdf.setFont('Newsreader', 'normal');
  pdf.text(`${purchaseOrder?.orderNumber}`, 155, 23);
  pdf.text(format(new Date(purchaseOrder?.orderDate), 'MMM dd, yyyy'), 155, 27);
  pdf.text(`${purchaseOrder?.poType}`, 155, 31);
  pdf.setFont('Newsreader', 'bold');
  pdf.text('To', 65, 23);
  pdf.setFont('Newsreader', 'normal');
  pdf.text(`${purchaseOrder?.vendorName}`, 65, 27);
  pdf.text(`${purchaseOrder?.vendorAddress}`, 65, 31);
  pdf.addImage(callImage, 'PNG', 65, 32, 3, 3);
  pdf.text(`${purchaseOrder?.vendorContact}`, 70, 35);
  pdf.text(`${purchaseOrder?.vendorMail}`, 65, 39);
  pdf.line(10, 42, 200, 42);


  // Define item details table headers
  const itemDetailsHeaders = [
    'Item Name',
    'Description',
    'Quantity',
    'Unit Price',
    'Total',
  ];
  pdf.setFont('courier');

  //  Define item details table rows
  const itemDetailsRows = demoData?.map((item: any) => [
    item.item_data?.item_name,
    item.item_data?.description,
    item.received_quantity,
    item.item_data?.rate,
    item.item_data?.rate * item.received_quantity,
  ]);
  const sumOfRates = demoData?.reduce((accumulator: any, currentItem: any) => {
    return (
      accumulator + currentItem.item_data?.rate * currentItem.received_quantity
    );
  }, 0);

  // // Create item details table
  const itemDetailsYStart = 50;
  pdf.autoTable({
    head: [itemDetailsHeaders],
    body: itemDetailsRows,
    startY: itemDetailsYStart, // Adjust the Y position as needed
  });
  const lastAutoTableHeight = pdf.lastAutoTable.finalY;


  pdf.setFont('Newsreader', 'bold');
  pdf.text('Total Amount   :', 150, lastAutoTableHeight + 7);
  pdf.setFont('Newsreader', 'normal');
  pdf.text(`${sumOfRates}`, 185, lastAutoTableHeight + 7);

  const summaryYStart = pdf.internal.pageSize.getHeight() - 50;

  pdf.setFont('Newsreader', 'bold');
  pdf.setFont('Newsreader', 'noraml');
  pdf.text('Thanking You,', 13, summaryYStart + 20);
  pdf.text('Yours Faithfully,', 13, summaryYStart + 24);
  pdf.text('For ', 13, summaryYStart + 28);
  pdf.setFont('Newsreader', 'bold');
  pdf.text('Eco Protection Engineers ', 19, summaryYStart + 28);
  const pageFooter = (pdf: any) => {
    const totalPages = pdf.internal.getNumberOfPages();
    for (let i = 1; i <= totalPages; i++) {
      pdf.line(10, 283, 200, 283);
      pdf.setPage(i);
      pdf.setFont('Newsreader');
      pdf.text(
        `Page ${i} of ${totalPages}`,
        185,
        pdf.internal.pageSize.getHeight() - 5
      );
    }
  };
  pageFooter(pdf);


  // Save the PDF
    pdf.save(`${purchaseOrder?.vendorName}_invoice.pdf`);

  // const pdfDataUri = pdf.output('datauristring');
  // const previewWindow = window.open();
  // previewWindow?.document.write(
  //   "<iframe width='100%' height='100%' src='" + pdfDataUri + "'></iframe>"
  // );


};

export default InvoiceReportGenerator;
