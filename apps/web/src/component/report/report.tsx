import React, { useState } from 'react';
import { Document, Page } from 'react-pdf';
import InvoiceGenerator from './templates/invoiceGenerator';

const TaxInvoice = () => {
  const invoiceData = {
    invoiceNumber: 'INV123',
    lineItems: [
      {
        id: 1,
        description:
          'Cutting, Benading of Steel for Raft & Wall, Making of Plywood Shuttering	',
        uom: 'LS',
        hsn: 'ASEDCF',
        quantity: '10',
        rate: '10',
        amount: '$100',
      },
      {
        id: 2,
        description:
          'Cutting, Benading of Steel for Raft & Wall, Making of Plywood Shuttering	',
        uom: 'LS',
        quantity: '10',
        rate: '10',
        amount: '$100',
      },
    ],
    total: '$250',
  };

  return (
    <div>
      <InvoiceGenerator invoiceData={invoiceData} />
    </div>
  );
};

export default TaxInvoice;
