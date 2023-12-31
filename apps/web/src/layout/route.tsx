// import React from 'react';
// import { Route, Routes } from 'react-router-dom';
// import Login from '../component/login';
// import Home from '../component/home';
// import ForgetPassword from '../component/forgetPassword';
// import ResetPassword from '../component/resetPassword/token';
// import ProtectedRoute from '../auth/ProtectedRoute';
// import Layout from './layout';
// import UserCreate from '../component/users/userCreate';
// import UserEdit from '../component/users/userEdit';
// import UserList from '../component/users/userList';
// import UserInformation from '../component/users/userInformation';
// import GstList from '../component/gst/gstList';
// import GstCreate from '../component/gst/gstCreate';
// import UomList from '../component/uom/uomList';
// import HsnCodeList from '../component/hsnCode/hsnCodeList';
// import ClientList from '../component/client/clientList';
// import CategoryList from '../component/category/categoryList';
// import SettingHome from '../component/settings/homeSetting';
// const route = () => {
//   return (
//     <div>
//       <Routes>
//         <Route path="/" element={<Login />} />
//         <Route path="/forget-password" element={<ForgetPassword />} />
//         <Route path="/reset-password/:id/:token" element={<ResetPassword />} />
//         <Route
//           path="/userList"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <UserList />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/userInfo/:id"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <UserInformation />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/home"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <Home />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/user-create"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <UserCreate />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/user-edit/:id"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <UserEdit />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/gst-list"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <GstList />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/gst-create"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <GstCreate />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/uom-list"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <UomList />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/hsncode-list"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <HsnCodeList />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/client-list"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <ClientList />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/category-list"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <CategoryList />
//             </ProtectedRoute>
//           }
//         />
//         <Route
//           path="/settings"
//           element={
//             <ProtectedRoute>
//               <Layout />
//               <SettingHome />
//             </ProtectedRoute>
//           }
//         />
//       </Routes>
//     </div>
//   );
// };

// export default route;
