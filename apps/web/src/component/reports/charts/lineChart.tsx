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
import { useState } from 'react';
import { Line } from 'react-chartjs-2';
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

const LineChart = () => {
  const data = {
    labels: ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'],
    datasets: [
      {
        label: 'Purchase Order',
        data: [0, 3, 4, 7, 15, 20],
        fill: false,
        backgroundColor: 'rgba(75,192,192,0.2)',
        borderColor: 'rgba(75,192,192,1)',
      },
      {
        label: 'Inwards',
        data: [0, 5, 3, 11, 14, 7],
        fill: false,
        borderColor: '#7752FE',
      },
      {
        label: 'QC Accepted',
        data: [0, 15, 2, 7, 19, 3],
        fill: false,
        borderColor: '#8E8FFA',
      },
    ],
  };
  const yAxisMin = 0; // Minimum value on the Y-axis
  const yAxisMax = 20.0;
  // Chart options
  const options = {
    min: yAxisMin,
    max: yAxisMax,
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
      <Line data={data} options={options} width={100} height={30} />
    </div>
  );
};

export default LineChart;
