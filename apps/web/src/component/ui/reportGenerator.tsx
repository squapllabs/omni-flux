import React from 'react';
import jsPDF from 'jspdf';

const ReportGenerator = () => {
  const generatePDF = () => {
    const doc = new jsPDF();
    doc.text('Purchase Order', 10, 10);

    // console.log("text");
    
    // Add more content to the PDF
    // For example:
    // doc.text('Item 1: Product A', 10, 20);
    // doc.text('Item 2: Product B', 10, 30);
    // ...

    // Save the PDF
    doc.save('purchase-order.pdf');
  };

  return (
    <div>
      <h1>Purchase Order</h1>
      <button onClick={generatePDF}>Generate PDF</button>
      {/* Add purchase order content here */}
    </div>
  );
}

export default ReportGenerator;


