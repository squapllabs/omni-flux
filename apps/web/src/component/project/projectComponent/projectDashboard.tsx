import React from 'react';
import Styles from '../../../styles/projectDashboard.module.scss';
import { formatBudgetValue } from '../../../helper/common-function';
import DashboardIcon from '../../menu/icons/dashboardIcon';
import { useParams } from 'react-router-dom';
import { useGetByProjectId } from '../../../hooks/project-hooks';
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

const ProjectDashboard = () => {
  const routeParams = useParams();
  const projectId = Number(routeParams?.id);
  const { data: getProjectData } = useGetByProjectId(projectId);

  const startDate = new Date(getProjectData?.date_started);
  const endDate = new Date(getProjectData?.date_ended);
  const currentDate = new Date();

  // Calculate the total duration of the project in days
  const projectDurationInMilliseconds = endDate - startDate;
  const totalDays = projectDurationInMilliseconds / (1000 * 60 * 60 * 24);

  // Calculate the time difference in milliseconds between the current date and start date
  const timeDifferenceInMilliseconds = currentDate - startDate;

  // Calculate the completed days count by dividing the time difference by the number of milliseconds in a day
  const completedDaysCount = Math.floor(
    timeDifferenceInMilliseconds / (1000 * 60 * 60 * 24)
  );
  const progress = currentDate - startDate;
  const progressInDays = progress / (1000 * 60 * 60 * 24);
  const progressPercentage =
    ((progressInDays / totalDays) * 100).toFixed(2) + '%';
  const data = {
    labels: ['Total Days', 'So Far'],
    datasets: [
      {
        label: 'total',
        data: [totalDays, completedDaysCount],
        backgroundColor: ['#190482', '#7752FE'],
        sorted: false,
      },
    ],
  };
  const options = {
    indexAxis: 'y',
    plugins: {
      legend: {
        display: false,
      },
    },

    scales: {
      x: {
        beginAtZero: true,
        grid: {
          display: false, // Hide X-axis gridlines
        },
      },
      y: {
        beginAtZero: true,
        grid: {
          display: false, // Hide X-axis gridlines
        },
      },
    },
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.subHeading}>
        <DashboardIcon width={25} height={30} />
        <h3>DASHBOARD</h3>
      </div>
      <div className={Styles.dashBoardcontainer}>
        <div className={Styles.headingContainer}></div>
        <div className={Styles.cardDiv}>
          <div>
            <div className={Styles.card}>
              <div className={Styles.textStyle}>
                <h4>
                  <b> Manager Name</b>
                </h4>
                <p>Vignesh AD</p>
              </div>
              <div className={Styles.textStyle}>
                <h4>
                  <b>Client Name</b>
                </h4>
                <p>Aalam Info Solutions LLP</p>
              </div>
              <div className={Styles.textStyle}>
                <h4>
                  <b> User Count</b>
                </h4>
                <p>18</p>
              </div>
              <div className={Styles.textStyle}>
                <h4>
                  <b> Site Count</b>
                </h4>
                <p>5</p>
              </div>
            </div>
          </div>

          <div className={Styles.projectProgress}>
            <span>PROJECT PROGRESS</span>
          </div>
          <div>
            <Bar width={80} height={15} data={data} options={options} />
          </div>
          <div>
            <span>BUDGET</span>
          </div>
          <div className={Styles.cardDiv}>
            <div
              style={{
                display: 'flex',
                gap: '100px',
              }}
            >
              <div className={Styles.card1}>
                <div className={Styles.textStyle}>
                  <h3>
                    <b>Estimated</b>
                  </h3>
                  <p>{formatBudgetValue(30000)}</p>
                </div>
              </div>
              <div className={Styles.card2}>
                <div className={Styles.textStyle}>
                  <h3>
                    <b>Actual</b>
                  </h3>
                  <p>{formatBudgetValue(30000)}</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
export default ProjectDashboard;
