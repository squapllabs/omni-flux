export const routes = [
  {
    url: '/hrms-api',
    auth: false,
    proxy: {
      target: process.env.HRMS_API_URL,
      changeOrigin: true,
    },
  },
  {
    url: '/api',
    auth: false,
    proxy: {
      target: process.env.BACKEND_API_URL,
      changeOrigin: true,
    },
  },
];

exports.routes = routes;
