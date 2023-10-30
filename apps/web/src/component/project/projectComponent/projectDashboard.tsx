import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/projectDashboard.module.scss';
import Card from '../../ui/CustomCard';
import { formatBudgetValue } from '../../../helper/common-function';
import DashboardIcon from '../../menu/icons/dashboardIcon';
import projectService from '../../../service/project-service';
import { useParams } from 'react-router-dom';


const ProjectDashboard = () => {
  const params = useParams();
  const [data, setData] = useState();
  // useEffect(() => {
  //   const fetchData = async () => {
  //     const getData = await projectService.getOneProjectById(
  //       Number(params?.id)
  //     );
  //     console.log("Get Data ==> ", getData);
  //     setData(getData?.data);
  //   }
  //   console.log("start date ==>", data?.date_started);
  //   // console.log("ISO start date ==>", data?.date_started?.toISOString());
  //   fetchData();
  // }, [params?.id]);  

  // const startedDate =  data?.date_started;
  // const sdate = new Date(startedDate);
  // const formattedStartedDate = sdate.toISOString().split('T')[0];

  // console.log("formattedStartedDate--->",formattedStartedDate);

  const startDate = new Date('2023-06-15');
  const endDate = new Date('2023-11-25');
  const currentDate = new Date();

  // Calculate the total duration of the project in days
  const projectDurationInMilliseconds = endDate - startDate;
  const totalDays = projectDurationInMilliseconds / (1000 * 60 * 60 * 24);

  const progress = currentDate - startDate;
  const progressInDays = progress / (1000 * 60 * 60 * 24);
  const progressPercentage =
    ((progressInDays / totalDays) * 100).toFixed(2) + '%';

  return (
    <div className={Styles.container}>
      <div className={Styles.subHeading}>
        <DashboardIcon width={25} height={30} />
        <h3>DASHBOARD</h3>
      </div>
      <div className={Styles.dashBoardcontainer}>
        <div className={Styles.headingContainer}>
        </div>
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
          <div className={Styles.border}>
            <div className={Styles.progressBar}>
              <div className={Styles.totalDays} style={{ width: `${totalDays}%` }}></div>
            </div>
            <div className={Styles.progressBar}>
              <div className={Styles.completedDays} style={{ width: `${progressPercentage}` }}></div>
            </div>
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
