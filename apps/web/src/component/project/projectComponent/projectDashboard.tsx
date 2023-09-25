import React from 'react'
import Styles from '../../../styles/projectDashboard.module.scss';
import Card from '../../ui/CustomCard';
import { Chart } from "react-google-charts";
import { formatBudgetValue } from '../../../helper/common-function';

const ProjectDashboard = () => {

    const startDate = new Date("2023-06-15");
    const endDate = new Date("2023-11-25");
    const currentDate = new Date();

    // Calculate the total duration of the project in days
    const projectDurationInMilliseconds = endDate - startDate;
    const totalDays = projectDurationInMilliseconds / (1000 * 60 * 60 * 24);

    const progress = currentDate - startDate;
    const progressInDays = progress / (1000 * 60 * 60 * 24);
    const progressPercentage = (((progressInDays / totalDays) * 100).toFixed(2) + "%");

    const chartOptions1 = {
        chart: {
            title: "Project Progress",
            subtitle: `Progress Percentage - ${progressPercentage}`,
        },
        title: 'Project Progress ',
        pieHole: 0.4,
        pieSliceTextStyle: {
            color: 'white',
        },
    }

    const projectStatusData = [
        ["Pipe Line Project", "Estimated Day", "Completed Day"],
        // [{projectStatus.top_projects[0].project name    }, 1000, 200],
        // ["Project Sample", new Date("2023-06-15"), new Date("2023-11-25")],
        ["Progress  ", totalDays, progressInDays],
        // ["Project 4", 540, 350],
    ];



    return (
        <div className={Styles.container}>
            <div className={Styles.dashBoardcontainer}>
                <div className={Styles.cardDiv}>
                    <div className={Styles.card}>
                        <div className={Styles.cardContainer}>
                            <div className={Styles.textStyle}>
                                <h3><b> Manager Name</b></h3>
                                <p>Vignesh AD</p>
                            </div>
                        </div>
                    </div>
                    <div className={Styles.card}>
                        <div className={Styles.cardContainer}>
                            <div className={Styles.textStyle}>
                                <h3><b>Client Name</b></h3>
                                <p>Aalam Info Solutions LLP</p>
                            </div>
                        </div>
                    </div>
                    <div className={Styles.card}>
                        <div className={Styles.cardContainer}>
                            <div className={Styles.textStyle}>
                                <h3><b> User Count</b></h3>
                                <p>18</p>
                            </div>
                        </div>
                    </div>
                    <div className={Styles.card}>
                        <div className={Styles.cardContainer}>
                            <div className={Styles.textStyle}>
                                <h3><b> Site Count</b></h3>
                                <p>5</p>
                            </div>
                        </div>
                    </div>
                </div>
                <div className={Styles.cardDiv1}>
                    <div className={Styles.graphCard}>
                        <div className={Styles.chart}>
                            <Chart
                                chartType="Bar"
                                height="215px"
                                data={projectStatusData}
                                options={chartOptions1}
                            />
                        </div>
                    </div>
                    <div className={Styles.card1}>
                        <div className={Styles.cardContainer1}>
                            <div className={Styles.textStyle}>
                                <h3><b>Estimated Budjet</b></h3>
                                <p>{formatBudgetValue(30000)}</p>
                            </div>
                       
                            <div className={Styles.textStyle}>
                                <h3><b>Actual Budjet</b></h3>
                                <p>{formatBudgetValue(30000)}</p>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

    )
}

export default ProjectDashboard