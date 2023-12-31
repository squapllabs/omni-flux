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

import React from 'react';
// import { Route, Routes } from 'react-router-dom';
import { Routes, Route } from 'react-router-dom';
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
import ProjectWorkBreakDownForm from '../component/projectWorkBreakDown/projectWorkBreakDownForm';
import ProjectWorkBreakDown from '../component/projectWorkBreakDown/projectWorkBreakDownList';
import ProjectWorkBreakDownEdit from '../component/projectWorkBreakDown/projectWorkBreakDownEdit';
import siteForm from '../component/site/siteForm';
import siteList from '../component/site/siteList';
import siteEdit from '../component/site/siteEdit';
import contractorList from '../component/contractor/contractorList';
import contractorForm from '../component/contractor/contractorForm';
import contractorEdit from '../component/contractor/contractorEdit';
import projectForm from '../component/project/projectForm';
import Lead from '../component/leadEnquires/leadList';
import leadEnquires from '../component/leadEnquires/leadEnquires';
import Settings from '../component/settings/homeSetting';
// import ExpansesForm from '../component/expanses/expansesForm';
import PopupExpanse from '../component/expanses/popupExpanse';
import projectList from '../component/project/projectList';
// import projectEdit from '../component/project/projectEdit';
import CategotyAdd from '../component/category/categoryAdd';
import SubCategoryAdd from '../component/subCategory/subCategoryAdd';
import SubsubCategoryAdd from '../component/subSubCategory/subsubcategoryAdd';
// import SubCategoryAdd from '../component/subCategory/subCatAdd';
import ProjectView from '../component/project/projectInfo';
import LeadInfoProduct from '../component/leadEnquires/leadInfoProduct';
import LeadInfoTender from '../component/leadEnquires/leadInfoTender';
// import BomList from '../component/bom/bomList';
// import Bom from '../component/bom/bom';
import Bom from '../component/projectBOQ/planList';
import ProductAdd from '../component/products/productAdd';
import LabourList from '../component/labour/labourList';
import LabourAdd from '../component/labour/labourAdd';
import AddMachinery from '../component/machinery/addMachinery';
import AddVendor from '../component/vendor/vendorAdd';
import ListVendor from '../component/vendor/vendorList';
import Project from '../component/project/project';
import ViewProject from '../component/vendor/vendorView';
import ProjectSettings from '../component/project/projectComponent/projectSettings';
import IndentRequest from '../component/project/projectComponent/projectIndentRequest/indentRequest';
import IndentView from '../component/indentApproval/indentList';
import PurchaseList from '../component/purchaseApproval/purchaseList';
import PurchaseView from '../component/purchaseApproval/purchaseView';
import IndentDetailView from '../component/indentApproval/indentView';
import VendorSelect from '../component/purchaseApproval/vendorSelect';
import PurchaseRequest from '../component/purchaseOrder/purchaseView';
import PurchaseOrderView from '../component/purchaseOrder/purchaseOrder';
import StockOutWardAdd from '../component/stockOutward/stockOutwardAdd';
import ProjectStockAdd from '../component/project/projectComponent/projectStockAdd';
import StockOutwardList from '../component/stockOutward/stockOutwardList';
import ProjectInventory from '../component/project/project-inventory';
import SiteExpensesForm from '../component/expanses/siteExpensesForm';
import VendorDetailsItemView from '../component/purchaseApproval/vendorDetailsItemView';
import PurchaseRequestAdd from '../component/purchaseApproval/purchaseRequestAdd';
import StockOutWardView from '../component/stockOutward/stockOutwardView';
import StockOutwardEdit from '../component/stockOutward/stockOutwardEdit';
import ProjectStockAuditView from '../component/project/projectComponent/projectStockAuditView';
import FinanceInvoiceView from '../component/finance/invoiceView';
import ExpenseApprove from '../component/project/projectComponent/projectSiteExpense/approval/siteExpenseApproveList';
import ExpenseDetailApprove from '../component/project/projectComponent/projectSiteExpense/approval/siteExpenseDetailApprove';
import BillView from '../component/finance/invoiceBillView';
import EcoProduction from '../component/ecoProduction/ecoProduction';
import projectAbstract from '../component/projectBOQ/projectAbstract';
// import NewBoqList from '../component/projectBOQ/projectBoqList';
import GenerateOtp from '../component/generateOtp';
import ExpenseRecall from '../component/expenseRecall/expenseRecall';
import PurchaseRequestItemView from '../component/purchaseApproval/purchaseRequestItemView';
import PurchaseOrderIndividualView from '../component/purchaseOrder/purchaseOrderView';
import PurchaseRequestList from '../component/purchaseRequest/purchaseRequestList';
import PurchaseOrderVendorSelect from '../component/purchaseApproval/purchaseVendorSelect';
import MyOrderList from '../component/project/projectComponent/myOrders/myOrdersList';
import MyOrderView from '../component/project/projectComponent/myOrders/orderView';
import BoqTaskListScreen from '../component/projectBOQ/taskScreen';
import LocalPurchaseList from '../component/project/projectComponent/localPurchase/localPurchaseList';
import LocalPurchaseOrder from '../component/project/projectComponent/localPurchase/localPurchaseOrder';
import DeliveryAddNote from '../component/project/projectComponent/myOrders/deliveryNoteAdd';
import ViewReceivedGoods from '../component/project/projectComponent/myOrders/viewReceivedGoods';
import VendorQuotesUpdate from '../component/purchaseRequest/vendorQuotesUpdate';
import Reports from '../component/reports/reports';
import FinanceInvoiceNewView from '../component/finance/invoiceNewView';
import AllPurchaseRequest from '../component/purchaseRequest/allPurchaseRequest/purchaseRequestAllList';
import ExpenseList from '../component/globalExpense/expenseClaimList';
const AppRoutes = () => {
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
  const ProtectedProjectWorkBreakDownFormPage = withLayoutAndProtection(
    ProjectWorkBreakDownForm
  );
  const ProtectedProjectWorkBreakDownListPage =
    withLayoutAndProtection(ProjectWorkBreakDown);
  const ProtectedProjectWorkBreakDownEditPage = withLayoutAndProtection(
    ProjectWorkBreakDownEdit
  );
  const ProtectedSiteFormPage = withLayoutAndProtection(siteForm);
  const ProtectedSitePage = withLayoutAndProtection(siteList);
  const ProtectedSiteEditPage = withLayoutAndProtection(siteEdit);
  const ProtectedContractorListPage = withLayoutAndProtection(contractorList);
  const ProtectedContractorFormPage = withLayoutAndProtection(contractorForm);
  const ProtectedContractorEditFormPage =
    withLayoutAndProtection(contractorEdit);
  const ProtectedProjectFormPage = withLayoutAndProtection(projectForm);
  const ProtectedSettings = withLayoutAndProtection(Settings);
  const ProtectedProjectListPage = withLayoutAndProtection(projectList);
  // const ProtectedProjectEdit = withLayoutAndProtection(projectEdit);
  const ProtectedLeadPage = withLayoutAndProtection(Lead);
  const ProtectedLeadEnquiresPage = withLayoutAndProtection(leadEnquires);
  const ProtectedProjectWorkBreakDownPage =
    withLayoutAndProtection(ProjectWorkBreakDown);
  const ProtectedCategoryAdd = withLayoutAndProtection(CategotyAdd);
  const ProtectedSubCategoryAdd = withLayoutAndProtection(SubCategoryAdd);
  const ProtectedSubSubCategoryAdd = withLayoutAndProtection(SubsubCategoryAdd);
  // const ProtectedExpanses = withLayoutAndProtection(ExpansesForm);
  const ProtectedPopupExpanse = withLayoutAndProtection(PopupExpanse);
  const ProtectedProjectView = withLayoutAndProtection(ProjectView);
  const ProtectedLeadProductView = withLayoutAndProtection(LeadInfoProduct);
  const ProtectedLeadTenderView = withLayoutAndProtection(LeadInfoTender);
  // const ProtectedBomList = withLayoutAndProtection(BomList);
  const ProtectedBomList = withLayoutAndProtection(projectAbstract);
  const ProtectedBoqTaskScreen = withLayoutAndProtection(BoqTaskListScreen);
  const ProtectedBom = withLayoutAndProtection(Bom);
  const ProtectedProductAddPage = withLayoutAndProtection(ProductAdd);
  const ProtectedLabourListPage = withLayoutAndProtection(LabourList);
  const ProtectedLabourAddPage = withLayoutAndProtection(LabourAdd);
  const ProtectedMachineryAddPage = withLayoutAndProtection(AddMachinery);
  const ProtectedVendorAddPage = withLayoutAndProtection(AddVendor);
  const ProtectedProject = withLayoutAndProtection(Project);
  const ProtectedVendorView = withLayoutAndProtection(ViewProject);
  const ProtectedProjectSettings = withLayoutAndProtection(ProjectSettings);
  const ProtectedIndent = withLayoutAndProtection(IndentRequest);
  const ProtectedIndentList = withLayoutAndProtection(IndentView);
  const ProtectedPurchaseList = withLayoutAndProtection(PurchaseList);
  const ProtectedIndentDetailView = withLayoutAndProtection(IndentDetailView);
  const ProtectedPurchaseDetailView = withLayoutAndProtection(PurchaseView);
  const ProtectedVendorSelect = withLayoutAndProtection(VendorSelect);
  const ProtectedPurchaseOrderView = withLayoutAndProtection(PurchaseRequest);
  const ProtectedPurchaseViewByvendor =
    withLayoutAndProtection(PurchaseOrderView);
  const ProtectedStockOutWardAdd = withLayoutAndProtection(StockOutWardAdd);
  // const ProtectedEcoProduction = withLayoutAndProtection(EcoProduction);

  const ProtectedStockOutwardList = withLayoutAndProtection(StockOutwardList);
  // const ProtectedProjectInventory = withLayoutAndProtection(ProjectInventory);
  // const ProtectedProjectInventory = withLayoutAndProtection(ProjectInventory);
  const ProtectedVendorDetailsItemView = withLayoutAndProtection(
    VendorDetailsItemView
  );
  const ProtectedPurchaseReuestAdd =
    withLayoutAndProtection(PurchaseRequestAdd);
  const ProtectedStockOutWardView = withLayoutAndProtection(StockOutWardView);
  const ProtectedStockOutwardEdit = withLayoutAndProtection(StockOutwardEdit);

  const ProtectedProjectStockAdd = withLayoutAndProtection(ProjectStockAdd);
  const ProtectedStoreOutwardList = withLayoutAndProtection(StockOutwardList);
  const ProtectedProjectStockAuditView = withLayoutAndProtection(
    ProjectStockAuditView
  );
  const ProtectedProjectInventory = withLayoutAndProtection(ProjectInventory);
  const ProtectedSiteExpensesForm = withLayoutAndProtection(SiteExpensesForm);
  const ProtectedFinanceInvoiceList =
    withLayoutAndProtection(FinanceInvoiceView);
  const ProtectedSiteExpensesApprove = withLayoutAndProtection(ExpenseApprove);
  const ProtectedExpenseDetailApprove =
    withLayoutAndProtection(ExpenseDetailApprove);
  const ProtectedBillView = withLayoutAndProtection(BillView);
  const ProtectedListVendor = withLayoutAndProtection(ListVendor);
  // const ProtectedNewBoqList = withLayoutAndProtection(NewBoqList);
  const ProtectedExpenseRecall = withLayoutAndProtection(ExpenseRecall);
  const ProtectedPurchaseRequestItemView = withLayoutAndProtection(
    PurchaseRequestItemView
  );
  const ProtectedPurchaseOrderIndividualView = withLayoutAndProtection(
    PurchaseOrderIndividualView
  );
  const ProtectedPurchaseRequestList =
    withLayoutAndProtection(PurchaseRequestList);
  const ProtectedPurchaseVendorSelect = withLayoutAndProtection(
    PurchaseOrderVendorSelect
  );
  const ProtectedMyOrders = withLayoutAndProtection(MyOrderList);
  const ProtectedOrderView = withLayoutAndProtection(MyOrderView);
  const ProtectedLocalPurchase = withLayoutAndProtection(LocalPurchaseList);
  const ProtectedLocalPurchaseOrder =
    withLayoutAndProtection(LocalPurchaseOrder);

  const ProtectedDeliveryNoteAdd = withLayoutAndProtection(DeliveryAddNote);
  const ProtectedViewReceivedGoods = withLayoutAndProtection(ViewReceivedGoods);
  const ProtectedVendorQuotesUpdate =
    withLayoutAndProtection(VendorQuotesUpdate);
  const ProtectedReports = withLayoutAndProtection(Reports);
  const ProtectedNewInvoiceView = withLayoutAndProtection(
    FinanceInvoiceNewView
  );
  const ProtectedAllPurchaseRequestList =
    withLayoutAndProtection(AllPurchaseRequest);
  const ProtectedExpenseList = withLayoutAndProtection(ExpenseList);
  return (
    <div>
      <Routes>
        <Route path="/" element={<Login />} />
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
        <Route
          path="/project-workbreakdown-add"
          element={<ProtectedProjectWorkBreakDownFormPage />}
        />
        <Route
          path="/project-workbreakdown"
          element={<ProtectedProjectWorkBreakDownListPage />}
        />
        <Route
          path="/project-workbreakdown-edit/:id"
          element={<ProtectedProjectWorkBreakDownEditPage />}
        />
        <Route path="/site-add" element={<ProtectedSiteFormPage />} />
        <Route path="/site" element={<ProtectedSitePage />} />
        <Route path="/site-edit/:id" element={<ProtectedSiteEditPage />} />
        <Route path="/contractor" element={<ProtectedContractorListPage />} />
        <Route
          path="/contractor-add"
          element={<ProtectedContractorFormPage />}
        />
        <Route
          path="/contractor-edit/:id"
          element={<ProtectedContractorEditFormPage />}
        />
        <Route path="/project-add" element={<ProtectedProjectFormPage />} />
        <Route
          path="/project-edit-draft/:id"
          element={<ProtectedProjectFormPage />}
        />
        <Route path="/lead-enquires" element={<ProtectedLeadPage />} />
        <Route path="/lead-add" element={<ProtectedLeadEnquiresPage />} />
        <Route
          path="/lead-edit/:id/:type"
          element={<ProtectedLeadEnquiresPage />}
        />
        <Route
          path="/project-workbreakdown"
          element={<ProtectedProjectWorkBreakDownPage />}
        />
        <Route path="/settings" element={<ProtectedSettings />} />
        <Route
          path="/expenses-edit/:projectId/:id"
          element={<ProtectedSiteExpensesForm />}
        />
        <Route
          path="/expenses/:projectId"
          element={<ProtectedSiteExpensesForm />}
        />
        <Route path="/expenses-popup" element={<ProtectedPopupExpanse />} />
        <Route path="/project-list" element={<ProtectedProjectListPage />} />
        {/* <Route path="/project-edit/:id" element={<ProtectedProjectEdit />} /> */}
        <Route path="/category-add" element={<ProtectedCategoryAdd />} />
        <Route path="/category-edit/:id" element={<ProtectedCategoryAdd />} />
        <Route path="/subcategory-add" element={<ProtectedSubCategoryAdd />} />
        <Route
          path="/subcategory-edit/:id"
          element={<ProtectedSubCategoryAdd />}
        />
        <Route
          path="/subsubcategory-add"
          element={<ProtectedSubSubCategoryAdd />}
        />
        <Route
          path="/subsubcategory-edit/:id"
          element={<ProtectedSubSubCategoryAdd />}
        />
        <Route path="/project-info/:id" element={<ProtectedProjectView />} />
        <Route
          path="/lead-info-product/:id"
          element={<ProtectedLeadProductView />}
        />
        <Route
          path="/lead-info-tender/:id"
          element={<ProtectedLeadTenderView />}
        />
        <Route
          path="/newBoq/:projectId/:bomconfigId"
          element={<ProtectedBomList />}
        />
        <Route
          path="/newBoq/:projectId/:bomconfigId/:categoryId"
          element={<ProtectedBoqTaskScreen />}
        />
        <Route path="/labour" element={<ProtectedLabourListPage />} />
        <Route path="/labour-add" element={<ProtectedLabourAddPage />} />
        <Route path="/labour-edit/:id" element={<ProtectedLabourAddPage />} />
        <Route path="/add-machinery" element={<ProtectedMachineryAddPage />} />
        <Route
          path="/edit-machinery/:id"
          element={<ProtectedMachineryAddPage />}
        />
        <Route path="/vendor-add" element={<ProtectedVendorAddPage />} />
        <Route path="/vendor-edit/:id" element={<ProtectedVendorAddPage />} />
        <Route path="/vendor-info/:id" element={<ProtectedVendorView />} />
        <Route path="/bom/:subCategoryId" element={<ProtectedBom />} />
        <Route path="/product-add" element={<ProtectedProductAddPage />} />
        <Route path="/product-edit/:id" element={<ProtectedProductAddPage />} />
        <Route path="/project" element={<ProtectedProject />} />
        <Route path="/project-edit/:id" element={<ProtectedProject />} />
        <Route
          path="/project-settings"
          element={<ProtectedProjectSettings />}
        />
        <Route path="/indent/:id" element={<ProtectedIndent />} />
        <Route path="/indent-view" element={<ProtectedIndentList />} />
        <Route
          path="/approved-indent-list"
          element={<ProtectedPurchaseList />}
        />
        <Route
          path="/indent-detail/:id"
          element={<ProtectedIndentDetailView />}
        />
        <Route
          path="/indent-request-detail/:id"
          element={<ProtectedPurchaseDetailView />}
        />
        <Route path="/indent/:id/:indentid" element={<ProtectedIndent />} />
        <Route path="/vendor-select/:id" element={<ProtectedVendorSelect />} />
        <Route
          path="/indent-detail/:id"
          element={<ProtectedIndentDetailView />}
        />
        {/* <Route path="/indent/:id/:indentid" element={<ProtectedIndent />} /> */}
        <Route
          path="/purchase-request/:id"
          element={<ProtectedPurchaseOrderView />}
        />
        <Route
          path="/purchase-order"
          element={<ProtectedPurchaseViewByvendor />}
        />
        <Route
          path="/stockoutward-add"
          element={<ProtectedStockOutWardAdd />}
        />
        <Route
          path="/project-stockadd/:id"
          element={<ProtectedProjectStockAdd />}
        />
        <Route path="/stockoutward" element={<ProtectedStockOutwardList />} />
        <Route
          path="/project-inventory/:id"
          element={<ProtectedProjectInventory />}
        />
        <Route
          path="/stockoutward-view/:id"
          element={<ProtectedStockOutWardView />}
        />
        <Route
          path="/stockoutward-edit/:id"
          element={<ProtectedStockOutwardEdit />}
        />
        <Route
          path="/project-inventory/:id"
          element={<ProtectedProjectInventory />}
        />
        <Route
          path="/vendor-view-items/:id"
          element={<ProtectedVendorDetailsItemView />}
        />
        <Route
          path="/purchase-request-add"
          element={<ProtectedPurchaseReuestAdd />}
        />
        <Route
          path="/stockoutward-view/:id"
          element={<ProtectedStockOutWardView />}
        />
        <Route
          path="/stockoutward-edit/:id"
          element={<ProtectedStockOutwardEdit />}
        />
        <Route path="/store-outward" element={<ProtectedStoreOutwardList />} />
        <Route
          path="/project-stockView/:id"
          element={<ProtectedProjectStockAuditView />}
        />
        <Route
          path="/project-inventory/:id"
          element={<ProtectedProjectInventory />}
        />
        <Route path="/finance-view" element={<ProtectedFinanceInvoiceList />} />
        <Route
          path="/site-expense-approve"
          element={<ProtectedSiteExpensesApprove />}
        />
        <Route
          path="/expense-detail-approve/:projectId/:id"
          element={<ProtectedExpenseDetailApprove />}
        />
        <Route path="/invoice-view/:id" element={<ProtectedBillView />} />
        <Route path="/eco-protection" element={<EcoProduction />} />
        <Route path="/vendor-list" element={<ProtectedListVendor />} />
        <Route path="/generate-otp" element={<GenerateOtp />} />
        <Route path="/expense-recall" element={<ProtectedExpenseRecall />} />
        <Route
          path="/request-items"
          element={<ProtectedPurchaseRequestItemView />}
        />
        <Route
          path="/purchase-order-view/:id"
          element={<ProtectedPurchaseOrderIndividualView />}
        />
        <Route
          path="/purchase-request-list/:id"
          element={<ProtectedPurchaseRequestList />}
        />
        <Route
          path="/vendor-select"
          element={<ProtectedPurchaseVendorSelect />}
        />
        <Route path="/my-orders" element={<ProtectedMyOrders />} />
        <Route path="/my-orders-view/:id" element={<ProtectedOrderView />} />
        <Route path="/local-purchase" element={<ProtectedLocalPurchase />} />
        <Route
          path="/local-purchase-order/:id"
          element={<ProtectedLocalPurchaseOrder />}
        />
        <Route
          path="delivery-note/:id"
          element={<ProtectedDeliveryNoteAdd />}
        />
        <Route path="/my-orders-view/:id" element={<ProtectedOrderView />} />
        <Route
          path="delivery-note/:id"
          element={<ProtectedDeliveryNoteAdd />}
        />
        <Route
          path="/view-received-goods/:pruchaseId/:grnId"
          element={<ProtectedViewReceivedGoods />}
        />
        <Route
          path="/vendor-quotes-update/:id"
          element={<ProtectedVendorQuotesUpdate />}
        />
        <Route path="/reports" element={<ProtectedReports />} />
        <Route path="/view-invoice/:id" element={<ProtectedNewInvoiceView />} />
        <Route
          path="/purchase-request-list-all"
          element={<ProtectedAllPurchaseRequestList />}
        />
        <Route path="/expense-list" element={<ProtectedExpenseList />} />
      </Routes>
    </div>
  );
};

export default AppRoutes;
