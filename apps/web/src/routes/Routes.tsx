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
import Category from '../component/category/category';
import ResetPassword from '../component/resetPassword/token';
import MasterData from '../component/masterData/masterData';
import ProjectWorkBreakDown from '../component/projectWorkBreakDown/projectWorkBreakDownForm';
import ProjectWorkBreakDownList from '../component/projectWorkBreakDown/projectWorkBreakDownList';
import ProjectWorkBreakDownEdit from '../component/projectWorkBreakDown/projectWorkBreakDownEdit';
import siteForm from '../component/site/siteForm';
import siteList from '../component/site/siteList';
import siteEdit from '../component/site/siteEdit';
import contractorList from '../component/contractor/contractorList';
import contractorForm from '../component/contractor/contractorForm';
import contractorEdit from '../component/contractor/contractorEdit';
import projectForm from '../component/project/projectForm';
import projectList from '../component/project/projectList';
import Settings from '../component/settings/homeSetting';
import projectEdit from '../component/project/projectEdit';

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
  const ProtectedCategoryPage = withLayoutAndProtection(Category);
  const ProtectedMasterDataPage = withLayoutAndProtection(MasterData);
  const ProtectedProjectWorkBreakDownPage = withLayoutAndProtection(ProjectWorkBreakDown);
  const ProtectedProjectWorkBreakDownListPage = withLayoutAndProtection(ProjectWorkBreakDownList);
  const ProtectedProjectWorkBreakDownEditPage = withLayoutAndProtection(ProjectWorkBreakDownEdit);
  const ProtectedSiteFormPage = withLayoutAndProtection(siteForm);
  const ProtectedSitePage = withLayoutAndProtection(siteList);
  const ProtectedSiteEditPage = withLayoutAndProtection(siteEdit);
  const ProtectedContractorListPage = withLayoutAndProtection(contractorList);
  const ProtectedContractorFormPage = withLayoutAndProtection(contractorForm);
  const ProtectedContractorEditFormPage = withLayoutAndProtection(contractorEdit);
  const ProtectedProjectFormPage = withLayoutAndProtection(projectForm);
  const ProtectedProjectListPage = withLayoutAndProtection(projectList);
  const ProtectedSettings = withLayoutAndProtection(Settings);
  const ProtectedProjectEdit = withLayoutAndProtection(projectEdit);

  return (
    <div>
      <Routes>
        <Route path="/" element={<Login setIsAuth={setIsAuth} />} />
        <Route path="/forget-password" element={<ForgetPassword />} />
        <Route path="/reset-password/:id/:token" element={<ResetPassword />} />
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
        <Route path="/category" element={<ProtectedCategoryPage />} />
        <Route path="/master-data" element={<ProtectedMasterDataPage />} />
        <Route path="/project-workbreakdown-add" element={<ProtectedProjectWorkBreakDownPage />} />
        <Route path="/project-workbreakdown" element={<ProtectedProjectWorkBreakDownListPage />} />
        <Route path="/project-workbreakdown-edit/:id" element={<ProtectedProjectWorkBreakDownEditPage />} />
        <Route path="/site-add" element={<ProtectedSiteFormPage />} />
        <Route path="/site" element={<ProtectedSitePage />} />
        <Route path="/site-edit/:id" element={<ProtectedSiteEditPage />} />
        <Route path="/contractor" element={<ProtectedContractorListPage />} />
        <Route path="/contractor-add" element={<ProtectedContractorFormPage />} />
        <Route path="/contractor-edit/:id" element={<ProtectedContractorEditFormPage />} />
        <Route path="/project-add" element={<ProtectedProjectFormPage />} />
        <Route path="/project-list" element={<ProtectedProjectListPage />} />
        <Route path="/project-workbreakdown" element={<ProtectedProjectWorkBreakDownPage />} />
        <Route path="/settings" element={<ProtectedSettings />} />
        <Route path="/project-edit/:id" element={<ProtectedProjectEdit />} />
      </Routes>
    </div>
  );
};

export default AppRoutes;
