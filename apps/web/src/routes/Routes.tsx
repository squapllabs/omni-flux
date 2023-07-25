// import React, { useState } from 'react';
// import { Route, Routes } from 'react-router-dom';
// import Login from '../component/login';
// import Home from '../component/home';
// import ForgetPassword from '../component/forgetPassword';

// import ProtectedRoute from '../auth/ProtectedRoute';

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
// import Layout from '../layout/layout';
// const AppRoutes = () => {
//   const [isAuth, setIsAuth] = useState<boolean>(false);
//   return (
//     <div>
//       <Routes>
//         <Route path="/" element={<Login setIsAuth={setIsAuth} />} />
//         <Route path="/forget-password" element={<ForgetPassword />} />

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
//       </Routes>
//     </div>
//   );
// };

// export default AppRoutes;

// import React, { useState } from 'react';
// import { Route, Routes } from 'react-router-dom';
// import Login from '../component/login';
// import Home from '../component/home';
// import ForgetPassword from '../component/forgetPassword';

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
// import Layout from '../layout/layout';
// import ProtectedRoute from '../auth/ProtectedRoute';

// const AppRoutes = () => {
//   // eslint-disable-next-line @typescript-eslint/no-unused-vars
//   const [isAuth, setIsAuth] = useState(false);

//   return (
//     <Routes>
//       <Route path="/" element={<Login setIsAuth={setIsAuth} />} />
//       <Route path="/forget-password" element={<ForgetPassword />} />

//       <ProtectedRoute>
//         <Layout>
//           <Routes>
//             <Route path="/home" element={<Home />} />
//             <Route path="/userList" element={<UserList />} />
//             <Route path="/userInfo/:id" element={<UserInformation />} />
//             <Route path="/user-create" element={<UserCreate />} />
//             <Route path="/user-edit/:id" element={<UserEdit />} />
//             <Route path="/gst-list" element={<GstList />} />
//             <Route path="/gst-create" element={<GstCreate />} />
//             <Route path="/uom-list" element={<UomList />} />
//             <Route path="/hsncode-list" element={<HsnCodeList />} />
//             <Route path="/client-list" element={<ClientList />} />
//             <Route path="/category-list" element={<CategoryList />} />
//           </Routes>
//         </Layout>
//       </ProtectedRoute>
//     </Routes>
//   );
// };

// export default AppRoutes;

import React, { useState } from 'react';
import { Route, Routes } from 'react-router-dom';
import Login from '../component/login';
import Home from '../component/home';
import ForgetPassword from '../component/forgetPassword';

import withLayoutAndProtection from '../hoc/withLayoutAndProtection';

import UserCreate from '../component/users/userCreate';
import UserEdit from '../component/users/userEdit';
import UserList from '../component/users/userList';
import UserInformation from '../component/users/userInformation';
import GstList from '../component/gst/gstList';
import GstCreate from '../component/gst/gstCreate';
import UomList from '../component/uom/uomList';
import HsnCodeList from '../component/hsnCode/hsnCodeList';
import AddProducts from '../component/products/addProducts';
import ClientList from '../component/client/clientList';
import CategoryList from '../component/category/categoryList';
import ProductPage from '../component/products/productPage';

const AppRoutes = () => {
  const [isAuth, setIsAuth] = useState<boolean>(false);

  const ProtectedHome = withLayoutAndProtection(Home);
  const ProtectedUserList = withLayoutAndProtection(UserList);
  const ProtectedUserInformation = withLayoutAndProtection(UserInformation);
  const ProtectedUserCreate = withLayoutAndProtection(UserCreate);
  const ProtectedUserEdit = withLayoutAndProtection(UserEdit);
  const ProtectedGstList = withLayoutAndProtection(GstList);
  const ProtectedGstCreate = withLayoutAndProtection(GstCreate);
  const ProtectedUomList = withLayoutAndProtection(UomList);
  const ProtectedHsnCodeList = withLayoutAndProtection(HsnCodeList);
  const ProtectedClientList = withLayoutAndProtection(ClientList);
  const ProtectedCategoryList = withLayoutAndProtection(CategoryList);
  const ProtectedAddProductPage = withLayoutAndProtection(AddProducts);
  const ProtectedProductPage = withLayoutAndProtection(ProductPage);

  return (
    <div>
      <Routes>
        <Route path="/" element={<Login setIsAuth={setIsAuth} />} />
        <Route path="/forget-password" element={<ForgetPassword />} />
        <Route path="/add-products" element={<ProtectedAddProductPage />} />
        <Route path="/products" element={<ProtectedProductPage />} />

        <Route path="/home" element={<ProtectedHome />} />
        <Route path="/userList" element={<ProtectedUserList />} />
        <Route path="/userInfo/:id" element={<ProtectedUserInformation />} />
        <Route path="/user-create" element={<ProtectedUserCreate />} />
        <Route path="/user-edit/:id" element={<ProtectedUserEdit />} />
        <Route path="/gst-list" element={<ProtectedGstList />} />
        <Route path="/gst-create" element={<ProtectedGstCreate />} />
        <Route path="/uom-list" element={<ProtectedUomList />} />
        <Route path="/hsncode-list" element={<ProtectedHsnCodeList />} />
        <Route path="/client-list" element={<ProtectedClientList />} />
        <Route path="/category-list" element={<ProtectedCategoryList />} />
      </Routes>
    </div>
  );
};

export default AppRoutes;
