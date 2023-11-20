import React from 'react';
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  BarElement,
  Title,
  Tooltip,
  Legend,
} from 'chart.js';
import { Bar } from 'react-chartjs-2';
ChartJS.register(
  CategoryScale,
  LinearScale,
  BarElement,
  Title,
  Tooltip,
  Legend
);
const StackedChart = () => {
  //
  const data = {
    labels: ['January', 'February', 'March', 'April', 'May'],

    datasets: [
      {
        label: 'Total',
        data: [2650, 2134, 1234, 2345, 2500],
        backgroundColor: '#190482', // Color for the first dataset
      },
      {
        label: 'On Time Delivery',
        data: [1912, 2321, 650, 900, 1900],
        backgroundColor: '#7752FE', // Color for the second dataset
      },
      {
        label: 'Late Delivered',
        data: [1000, 1456, 785, 3500, 2000],
        backgroundColor: '#8E8FFA', // Color for the second dataset
      },
      {
        label: 'Pending',
        data: [1245, 999, 2225, 2340, 1745],
        backgroundColor: '#C2D9FF', // Color for the second dataset
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
  return (
    <div>
      <div>
        <Bar data={data} options={options} width={100} height={40} />
      </div>
    </div>
  );
};

export default StackedChart;
