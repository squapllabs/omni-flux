import React from 'react';
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
const HorizontalBarChart = () => {
  const data = {
    labels: [
      'All POs',
      'Sent for Approval',
      'Approved POs',
      'Rejected POs',
      'Approval Pending',
    ],
    datasets: [
      {
        label: 'POs',
        data: [10, 20, 30, 40, 50],
        backgroundColor: 'rgba(75, 192, 192, 0.6)',
      },
    ],
  };
  const options = {
    indexAxis: 'y',
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
    <div>
      <Bar data={data} options={options} width={50} height={40} />
    </div>
  );
};

export default HorizontalBarChart;
