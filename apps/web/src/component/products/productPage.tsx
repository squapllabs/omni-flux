import React, { useState } from 'react';
import Button from '../menu/button';
import { useNavigate } from 'react-router-dom';
import Styles from '..//../styles/addProduct.module.scss';
// import './productPage.css';
import AddIcon from '../menu/icons/addIcon';
import DownloadIcon from '../menu/icons/download';
import Dropdown from '../menu/dropDown';
import CancelFilterIcon from '../menu/icons/cancelFilterIcon';
import FilterIcon from '../menu/icons/filterIcon';
import DescendingIcon from '../menu/icons/descendingIcon';
const ProductPage = () => {
  const navigate = useNavigate();
  const handleAddProduct = () => {
    navigate('/add-products');
  };
  const handleDownload = () => {
    // Create the CSV content
    const csvContent =
      'Name,Email\nJohn Doe,johndoe@example.com\nJane Smith,janesmith@example.com';
    // Create a Blob with the CSV content
    const blob = new Blob([csvContent], { type: 'text/csv' });
    // Create a temporary URL for the Blob
    const url = URL.createObjectURL(blob);
    // Create an anchor element
    const link = document.createElement('a');
    link.href = url;
    link.download = 'data.csv';
    link.click();
    URL.revokeObjectURL(url);
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.topContent}>
        <div className={Styles.leftContainer}>
          <h1>List of Items</h1>
          <p>Manage your raw materials ( Raw, Semi Finished & Finished).</p>
        </div>
        <div className={Styles.rightContainer}>
          <div className={Styles.button}>
            <Button
              text={
                <div style={{ display: 'flex', alignItems: 'center' }}>
                  <DownloadIcon style={{ padding: '4px' }} />
                  Download csv
                </div>
              }
              onClick={handleDownload}
              backgroundColor="white"
              textColor="black"
              width={140}
              border="1px solid #D0D5DD"
              borderRadius={8}
            />
          </div>
          <div className={Styles.button}>
            <Button
              text={
                <div style={{ display: 'flex', alignItems: 'center' }}>
                  <AddIcon style={{ padding: '4px' }} />
                  Add items
                </div>
              }
              onClick={handleAddProduct}
              backgroundColor="#7F56D9"
              fontSize={14}
              fontWeight={500}
              width={125}
            />
          </div>
        </div>
      </div>
      <div className={Styles.dropdownButton}>
        <div className={Styles.button}>
          <Dropdown
            label={
              <Button
                text={
                  <div
                    style={{
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'center',
                    }}
                  >
                    <FilterIcon style={{ padding: '0 10px' }} />
                    <p style={{ padding: '0 10px', fontSize: '16px' }}>
                      Filter
                    </p>
                    <CancelFilterIcon
                      style={{
                        padding: '8px 10px',
                        borderLeft: '1px solid #D0D5DD',
                      }}
                    />
                  </div>
                }
                onClick={() => {}}
                width={150}
                textColor="black"
                backgroundColor="white"
                border="1px solid #D0D5DD"
                borderRadius={8}
              />
            }
          >
            <div>filter item</div>
          </Dropdown>
        </div>
        <div className="button">
          <Dropdown
            label={
              <Button
                text={
                  <div
                    style={{
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'center',
                    }}
                  >
                    <DescendingIcon style={{ padding: '0 10px' }} />
                    <p
                      style={{
                        padding: '8px 10px',
                        fontSize: '16px',
                        borderLeft: '1px solid #D0D5DD',
                      }}
                    >
                      Last Updated On
                    </p>
                  </div>
                }
                onClick={() => {}}
                width={190}
                textColor="black"
                backgroundColor="white"
                border="1px solid #D0D5DD"
                borderRadius={8}
              />
            }
          >
            <div></div>
          </Dropdown>
        </div>
      </div>
    </div>
  );
};

export default ProductPage;