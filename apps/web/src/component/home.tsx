import React, { useState } from 'react';
import { useGetAllUsers } from '../hooks/user-hooks';
import Styles from '../styles/home.module.scss';
import Button from './menu/button';
import Vector from './menu/icons/vector';
import FolderIcon from './menu/icons/folderIcon';
const Home = () => {
  const { data: getAllUsers } = useGetAllUsers();
  // console.log('getAllUsers', getAllUsers);
  const [createItem, setCreateItem] = useState(true);
  const [createCustomer, setCreateCustomer] = useState(false);
  const handleCreateList = () => {
    setCreateItem(true);
    setCreateCustomer(false);
  };

  const handleCreateCustomer = () => {
    setCreateCustomer(true);
    setCreateItem(false);
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.homeContainer}>
        <div className={Styles.homeLeftContent}>
          <h2>Let's begin your journey with OmniFlux ERP</h2>
          <p>Item,customer,Supplier,Quatation</p>
          <h1
            className={`${Styles.selectionList} ${
              createItem ? Styles.active : ''
            }`}
            onClick={handleCreateList}
          >
            1 Create an item
          </h1>
          <h1
            className={`${Styles.selectionList} ${
              createCustomer ? Styles.active : ''
            }`}
            onClick={handleCreateCustomer}
          >
            2 Create a Customer
          </h1>
        </div>
        {createItem && (
          <div className={Styles.homeRightContent}>
            <h1>Create an Item</h1>
            <p>
              Item is a product, of a or service offered by your company, or
              something you buy as a part of your supplies or raw materials.
            </p>
            <p>
              Items are integral to everything you do in ERPNext - from billing,
              purchasing to managing inventory. Everything you buy or sell,
              whether it is a physical product or a service is an Item. Items
              can be stock, non-stock, variants, serialized, batched, assets
              etc.
            </p>
            <h2>Show Item List</h2>
            <Button
              text="Get started"
              onClick={() => {
                console.log('getting started');
              }}
              backgroundColor="#7F56D9"
              width="24%"
              borderRadius={8}
            />
          </div>
        )}

        {createCustomer && (
          <div className={Styles.homeRightContent}>
            <h1>Create an Customer</h1>
            <p>
              Item is a product, of a or service offered by your company, or
              something you buy as a part of your supplies or raw materials.
            </p>
            <p>
              Items are integral to everything you do in ERPNext - from billing,
              purchasing to managing inventory. Everything you buy or sell,
              whether it is a physical product or a service is an Item. Items
              can be stock, non-stock, variants, serialized, batched, assets
              etc.
            </p>
            <h2>Show Customer List</h2>
            <Button
              text="Get started"
              onClick={() => {
                console.log('getting started');
              }}
              backgroundColor="#7F56D9"
              width="20%"
            />
          </div>
        )}
      </div>

      <div className={Styles.shortcutsContainer}>
        <div className={Styles.title}>Your Shortcuts</div>

        <div className={Styles.shortcuts}>
          <p className={Styles.reportItemsStart}>
            Item <Vector width={10} height={12} style={{ padding: '0 12px' }} />
          </p>

          <p className={Styles.reportItems}>
            Customer{' '}
            <Vector width={10} height={12} style={{ padding: '0 12px' }} />
          </p>

          <p className={Styles.reportItems}>
            Supplier
            <Vector width={10} height={12} style={{ padding: '0 12px' }} />
          </p>

          <p className={Styles.reportItemsEnd}>
            Sale Invoice
            <Vector width={10} height={12} style={{ padding: '0 12px' }} />
          </p>
        </div>
        <div className={Styles.title}> Reports & Masters</div>

        <div className={Styles.reportContainer}>
          <div>
            <FolderIcon
              height={20}
              width={16}
              style={{ padding: '0 12px 0 0' }}
            />
            <span className={Styles.reportHeading}>Stock</span>
            <div>
              <p>Item</p>
              <p>Warehouse</p>
              <p>Brand</p>
              <p>Unit of Measurment (UOM)</p>
              <p>Stock Reconcilation</p>
            </div>
          </div>
          <div>
            <FolderIcon
              height={20}
              width={16}
              style={{ padding: '0 12px 0 0' }}
            />
            <span className={Styles.reportHeading}>Sales</span>
            <div>
              <p>Item</p>
              <p>Warehouse</p>
              <p>Brand</p>
              <p>Unit of Measurment (UOM)</p>
              <p>Stock Reconcilation</p>
            </div>
          </div>
          <div>
            <FolderIcon
              height={20}
              width={16}
              style={{ padding: '0 12px 0 0' }}
            />
            <span className={Styles.reportHeading}>Expense</span>
            <div>
              <p>Item</p>
              <p>Warehouse</p>
              <p>Brand</p>
              <p>Unit of Measurment (UOM)</p>
              <p>Stock Reconcilation</p>
            </div>
          </div>
        </div>
        <div className={Styles.title}> Data Import & Settings</div>
        <div className={Styles.importContainer}>
          <p>Import Data</p>
        </div>
      </div>
    </div>
  );
};

export default Home;
