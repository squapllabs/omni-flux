import React, { useEffect, useState } from 'react';
import Styles from '../styles/home.module.scss';
import Button from './menu/button';
import Vector from './menu/icons/vector';
import FolderIcon from './menu/icons/folderIcon';
import CustomCard from './ui/CustomCard';
import CustomLoader from './ui/customLoader';
import { formatBudgetValue } from '../helper/common-function';
import {
  useGetAllProject,
  useGetAllProjectStatus,
  useGetDashboardDatasforPO,
} from './../hooks/project-hooks';
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  BarElement,
  Title,
  Tooltip,
  Legend,
  ArcElement,
  PointElement,
  LineController,
  registerables,
} from 'chart.js';
import { Bar } from 'react-chartjs-2';
ChartJS.register(
  CategoryScale,
  LinearScale,
  BarElement,
  ArcElement,
  Title,
  Tooltip,
  Legend,
  PointElement,
  LineController,
  ...registerables
);

const Home = () => {
  const [createItem, setCreateItem] = useState(true);
  const [createCustomer, setCreateCustomer] = useState(false);
  const { isLoading: getAllLoading } = useGetAllProject();
  const { data: projectStatus = [] } = useGetAllProjectStatus();
  const { data: projectData = [], isLoading: getAllProjectStatusLoading } =
    useGetDashboardDatasforPO();

  const projectNames: any = [];
  const projectTotal: any = [];
  const projectCompleted: any = [];
  projectData?.top_projects?.map(async (val: any) => {
    projectNames.push(val.project_name);
    projectTotal.push(val.project_total_days);
    projectCompleted.push(val.days_completed);
  });

  const data = {
    labels: projectNames,

    datasets: [
      {
        label: 'Total Days',
        data: !getAllProjectStatusLoading ? projectTotal : [],
        backgroundColor: '#190482', // Color for the first dataset
      },
      {
        label: 'So Far',
        data: !getAllProjectStatusLoading ? projectCompleted : [],
        backgroundColor: '#7752FE', // Color for the second dataset
      },
    ],
  };

  const yAxisMin = 0; // Minimum value on the Y-axis
  const yAxisMax = 3000;
  // Chart options
  const options = {
    min: yAxisMin,
    max: yAxisMax,
    scales: {
      x: {
        grid: {
          display: false, // Hide X-axis gridlines
        },
      },
      y: {
        grid: {
          display: false, // Hide X-axis gridlines
        },
      },
    },
  };

  const topProjectsData: any = [['Projects', 'Budget']];
  projectData?.top_projects?.map(async (val: any) => {
    await topProjectsData.push([val.project_name, val.total_budget]);
  });

  const formatNumberToLakh = (number: number) => {
    const lakhValue = number / 100000;
    const formattedNumber = '₹ ' + lakhValue.toFixed(1) + ' L';
    return formattedNumber;
  };

  return (
    <div className={Styles.container1}>
      <CustomLoader
        loading={getAllProjectStatusLoading}
        size={48}
        color="#333C44"
      >
        <div className={Styles.containerCard}>
          <div>PROJECT TRACKER</div>
          <div className={Styles.barCarddDiv}>
            <div className={Styles.chart}>
              <Bar width={50} height={15} data={data} options={options} />
            </div>
          </div>
          <div>
            PROJECTS
            <div className={Styles.cardDiv}>
              <div className={Styles.cardContainer}>
                <div className={Styles.cardTextStyle}>
                  <h3>
                    <b>Inprogress</b>
                  </h3>
                  <div className={Styles.textStyle1}>
                    {projectData?.inprogress_projects}
                  </div>
                </div>
                <div className={Styles.cardTextStyle}>
                  <h3>
                    <b>Yet to Start</b>
                  </h3>
                  <p className={Styles.textStyle1}>
                    {projectData?.draft_projects}
                  </p>
                </div>
                <div className={Styles.cardTextStyle}>
                  <h3>
                    <b>Active</b>
                  </h3>
                  <p className={Styles.textStyle2}>
                    {projectData?.active_projects}
                  </p>
                </div>
              </div>
              <div className={Styles.cardContainer1}>
                <div className={Styles.cardTextStyle}>
                  <h3>
                    <b>Completed</b>
                  </h3>
                  <p className={Styles.textStyle1}>
                    {projectData?.completed_projects}
                  </p>
                </div>
              </div>
              <div className={Styles.cardContainer2}>
                <div className={Styles.cardTextStyle}>
                  <h3>
                    <b>Total</b>
                  </h3>
                  <p className={Styles.textStyle3}>
                    {projectData?.total_projects}
                  </p>
                </div>
              </div>
            </div>
          </div>
          <div className={Styles.centerCard}>
            {/* <div className={Styles.dashedLine}></div>
            <div className={Styles.headingGap}>
              CUSTOMERS (57)
              <div className={Styles.centerFirst}>
                <div className={Styles.centerGap}>
                  <h3>Total Invoiced</h3>
                  <div>
                    <b>₹14.32 L</b> (YTD)
                  </div>
                </div>
                <div className={Styles.centerGap}>
                  <h3> Received</h3>
                  <div>
                    <b>-</b>
                  </div>
                </div>
                <div className={Styles.centerGap}>
                  <h3>Outstanding</h3>
                  <div>
                    <b>₹22.69 L</b> (YTD)
                  </div>
                </div>
              </div>
            </div> */}
            <div className={Styles.dashedLine}></div>
            <div className={Styles.headingGap}>
              {`VENDORS (${projectData?.active_vendors})`}
              <div className={Styles.centerFirst}>
                <div className={Styles.centerGap}>
                  <h3>Total Invoiced</h3>
                  <div>
                    <b>
                      {formatNumberToLakh(
                        projectData?.total_purchase_order_statistics
                          ?.total_purchase_order_cost
                      )}
                    </b>
                  </div>
                </div>
                <div className={Styles.centerGap}>
                  <h3> Paid</h3>
                  <div>
                    <b>
                      {formatNumberToLakh(
                        projectData?.total_purchase_order_statistics
                          ?.total_cost_completed
                      )}
                    </b>
                  </div>
                </div>
                <div className={Styles.centerGap}>
                  <h3>Pending</h3>
                  <div>
                    <b>
                      {formatNumberToLakh(
                        projectData?.total_purchase_order_statistics
                          ?.total_cost_other_than_completed
                      )}
                    </b>
                  </div>
                </div>
              </div>
            </div>
            <div className={Styles.dashedLine}></div>
            <div className={Styles.headingGap}>
              PURCHASE
              <div className={Styles.centerFirst}>
                <div className={Styles.centerGap}>
                  <h3>Pending</h3>
                  <div>
                    <b>
                      {projectData?.total_purchase_order_statistics?.pending_po}
                    </b>
                  </div>
                </div>
                <div className={Styles.centerGap}>
                  <h3> Completed</h3>
                  <div>
                    <b>
                      {
                        projectData?.total_purchase_order_statistics
                          ?.completed_po
                      }
                    </b>
                  </div>
                </div>
              </div>
            </div>
            <div className={Styles.dashedLine}></div>
          </div>
        </div>
        <div className={Styles.projectDiv}>
          <div className={Styles.headingGap}>
            TOP 5: PROJECTS
            <table className={Styles.scrollable_table}>
              <thead></thead>
              <tbody>
                {projectData?.top_projects?.map((data: any, index: any) => {
                  return (
                    <tr key={data?.project_name}>
                      <td className={Styles.headTitle}>
                        {data?.project_name}
                        {/* <span className={Styles.spantag}>120 invoices (so for)</span> */}
                      </td>
                      <td className={Styles.budget}>
                        {formatNumberToLakh(
                          data?.total_budget ? data?.total_budget : 0
                        )}
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
          <div className={Styles.projectDiv2}>
            {' '}
            <div className={Styles.projectPayment}>
              TOP 5: PAYMENT OUTSTANDING
            </div>
            <table className={Styles.scrollable_table}>
              <tbody>
                {projectData?.project_based_purchase_order_data?.map(
                  (data: any) => {
                    return (
                      <tr key={data?.project_name}>
                        <td>
                          <span className={Styles.headTitle}>
                            {data?.project_name}
                          </span>

                          <span className={Styles.spantag}>
                            {data?.count_of_pending_po} invoices (so for)
                          </span>
                        </td>
                        <td className={Styles.budget}>
                          {formatNumberToLakh(
                            data?.total_cost_other_than_completed
                          )}
                        </td>
                      </tr>
                    );
                  }
                )}
              </tbody>
            </table>
          </div>
          {/* <div className={Styles.projectDiv3}>PURCHASE
              <table className={Styles.scrollable_table}>
                <tbody>
                  <tr>
                    <td> PRODUCT RECEIVED
                    </td>
                    <td className={Styles.budget}>98</td>
                  </tr>
                  <tr>
                    <td> PENDING
                    </td>
                    <td className={Styles.budget}>33</td>
                  </tr>
                  <tr>
                    <td>
                      COMPLETED
                    </td>
                    <td className={Styles.budget}>65</td>
                  </tr>
                </tbody>
              </table>


            </div> */}
        </div>

        {/* <div className={Styles.homeContainer}>
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
        </div> */}
      </CustomLoader>
    </div>
  );
};

export default Home;
