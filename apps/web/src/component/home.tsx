import React, { useState } from 'react';
import Styles from '../styles/home.module.scss';
import Button from './menu/button';
import Vector from './menu/icons/vector';
import FolderIcon from './menu/icons/folderIcon';
import CustomCard from './ui/CustomCard';
import { Chart } from "react-google-charts";
import CustomLoader from './ui/customLoader';
import {
  useGetAllProject,
  useGetAllProjectStatus,
} from './../hooks/project-hooks';

const Home = () => {
  const [createItem, setCreateItem] = useState(true);
  const [createCustomer, setCreateCustomer] = useState(false);
  const { isLoading: getAllLoading } = useGetAllProject();
  const { data: projectStatus, isLoading: getAllProjectStatusLoading } = useGetAllProjectStatus();


  const handleCreateList = () => {
    setCreateItem(true);
    setCreateCustomer(false);
  };

  const handleCreateCustomer = () => {
    setCreateCustomer(true);
    setCreateItem(false);
  };


  const projectStatusData: any = [["Projects", "Total Days", "So Far"]];
  projectStatus?.top_projects?.map(async (val: any) => {
    await projectStatusData.push([val.project_name, val.project_total_days, val.days_completed])
  });

  const topProjectsData: any = [["Projects", "Budget"]];
  projectStatus?.top_projects?.map(async (val: any) => {
    await topProjectsData.push([val.project_name, val.total_budget])
  });


  const chartOptions1 = {
    hAxis: {
      title: 'Days',
      minValue: 0,
    },
    vAxis: {
      title: 'Project Name',
      textStyle: {
        fontSize: 0.1
      },
      viewWindow: {
        min: 10,
      },
    },
    legend: { position: 'none' },
    series: {
      0: {
        targetAxisIndex: 0,
      },
    },
    colors: ['#6941C6', '#32D583']
  };


  return (

    <div className={Styles.container1}>
      <CustomLoader
        loading={getAllProjectStatusLoading === false ? getAllProjectStatusLoading : projectStatus}
        size={48}
        color="#333C44"
      >
        <div className={Styles.containerCard}>
          {/* <div className={Styles.dashBoardcontainer}> */}
            <div>PROJECT TRACKER</div>
            <div className={Styles.barCarddDiv}>
              <div className={Styles.chart}>
                <Chart
                  chartType="Bar"
                  height="250px"
                  data={projectStatusData}
                  options={chartOptions1}
                />
              </div>
            </div>
            <div >PROJECTS
            <div className={Styles.cardDiv}>
              <div className={Styles.cardContainer}>
                <div className={Styles.cardTextStyle}>
                  <h3><b>Inprogress</b></h3>
                  <div className = {Styles.textStyle1}>{projectStatus?.inprogress_projects}</div>
                </div>
                <div className={Styles.cardTextStyle}>
                  <h3><b>Yet to Start</b></h3>
                  <p className={Styles.textStyle1}>{projectStatus?.not_started_projects}</p>
                </div>
                <div className={Styles.cardTextStyle}>
                  <h3><b>Active</b></h3>
                  <p className={Styles.textStyle2}>{projectStatus?.active_projects}</p>
                </div>
              </div>
              <div className={Styles.cardContainer1}>
                <div className={Styles.cardTextStyle}>
                  <h3><b>Completed</b></h3>
                  <p className={Styles.textStyle1}>{projectStatus?.completed_projects}</p>
                </div>
              </div>
              <div className={Styles.cardContainer2}>
                <div className={Styles.cardTextStyle}>
                  <h3><b>Total</b></h3>
                  <p className={Styles.textStyle3}>{projectStatus?.total_projects}</p>
                </div>
              </div>
            </div>
          </div>
        </div>

        <div className={Styles.homeContainer}>
          <div className={Styles.homeLeftContent}>
            <h2>Let's begin your journey with OmniFlux ERP</h2>
            <p>Item,customer,Supplier,Quatation</p>
            <h1
              className={`${Styles.selectionList} ${createItem ? Styles.active : ''
                }`}
              onClick={handleCreateList}
            >
              1 Create an item
            </h1>
            <h1
              className={`${Styles.selectionList} ${createCustomer ? Styles.active : ''
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
                backgroundColor="#1A5D1A"
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
                backgroundColor="#1A5D1A"
                width="24%"
                borderRadius={8}
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
              <div className={Styles.items}>
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
              <div className={Styles.items}>
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
              <div className={Styles.items}>
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
      </CustomLoader>
    </div>

  );
};

export default Home;
