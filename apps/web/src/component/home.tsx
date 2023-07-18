import React from 'react';
import { useGetAllUsers } from '../hooks/user-hooks';
import Styles from '../styles/home.module.scss';
import Button from './menu/button';
const Home = () => {
  const { data: getAllUsers } = useGetAllUsers();
  // console.log('getAllUsers', getAllUsers);

  return (
    <div className={Styles.container}>
      <div className={Styles.homeContainer}>
        <div className={Styles.homeLeftContent}>
          <h2>Let's begin your journey with OmniFlux ERP</h2>
          <p>Item,customer,Supplier,Quatation</p>
          <h1>1 Create an item</h1>
          <h1>2 Create an Customer</h1>
        </div>
        <div className={Styles.homeRightContent}>
          <h1>Create an Item</h1>
          <p>
            Item is a product, of a or service offered by your company, or
            something you buy as a part of your supplies or raw materials.
          </p>
          <p>
            Items are integral to everything you do in ERPNext - from billing,
            purchasing to managing inventory. Everything you buy or sell,
            whether it is a physical product or a service is an Item. Items can
            be stock, non-stock, variants, serialized, batched, assets etc.
          </p>
          <h2>Show Item List</h2>
          <Button
            text="Get started"
            onClick={() => {}}
            backgroundColor="#7F56D9"
            width="20%"
          />
        </div>
      </div>
    </div>
  );
};

export default Home;
