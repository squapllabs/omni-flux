import React, { useEffect, useState } from 'react';
import CustomGroupButton from '../ui/CustomGroupButton';
import Button from '../ui/Button';
import Styles from '../../styles/newStyles/project.module.scss';
import ProjectGeneralDetails from './projectComponent/projectGeneralDetails';
import ProjectDashboard from './projectComponent/projectDashboard';
import ProjectBomConfig from './projectComponent/projectBomConfig';
import ProjectSiteConfig from './projectComponent/projectSiteConfig';
import CustomLoader from '../ui/customLoader';
import { useParams, useNavigate } from 'react-router-dom';
import { getByProjectId } from '../../hooks/project-hooks';
import projectService from '../../service/project-service';
import ProjectSettings from './projectComponent/projectSettings';
import ProjectDocument from './projectComponent/projectDocument';
import IndentRequest from './projectComponent/projectIndentRequest/indentRequest';
import ProjectIndentRequestList from './projectComponent/projectIndentRequest/projectIndentRequestList';
import ProjectStockmanagement from './projectComponent/projectStockmanagement';
import SiteExpenseList from '../expanses/siteExpenseList';
import ProjectStockOutward from '../stockOutward/stockOutwardList';
import ProjectMasterData from './projectComponent/projectMasterData';
import KeyboardBackspaceIcon from '../menu/icons/backArrow';
import FirstPageIcon from '../menu/icons/firstPageIcon';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
import SideNav from '../ui/sideNav';
import ProjectSiteExpenseList from './projectComponent/projectSiteExpense/projectSiteExpenseList';
import { useDispatch } from 'react-redux';
import { setToken, getToken } from '../../redux/reducer';
import { store, RootState } from '../../redux/store';
import MyOrders from './projectComponent/myOrders/myOrdersList';
import LocalPurchaseList from './projectComponent/localPurchase/localPurchaseList';
import Store from './project-inventory';

const Project = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const state: RootState = store.getState();
  const projectMenuID = getToken(state, 'projectMenuID');
  // const projectMenuID: any = encryptedData;
  console.log('projectMenuID', projectMenuID);

  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Dashboard', value: 'PDB' },
    { label: 'Generic', value: 'PGS' },
    { label: 'Settings', value: 'PSG' },
    { label: 'Site', value: 'PSC' },
    { label: 'BOQ', value: 'PBC' },
    { label: 'Document', value: 'PDT' },
    { label: 'Indent', value: 'PIR' },
    { label: 'Stock Audit', value: 'PSM' },
    { label: 'Site Expense', value: 'PSE' },
    { label: 'Stock Outward', value: 'PSO' },
    { label: 'Master Data', value: 'MD' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('PGS');
  const [loader, setLoader] = useState(false);
  const [projectData, setProjectData] = useState<any>({});
  useEffect(() => {
    const fetchData = async () => {
      const getData = await projectService.getOneProjectById(
        Number(routeParams?.id)
      );
      setProjectData(getData);
    };
    if (routeParams?.id != undefined) fetchData();
  }, [loader]);
  const handleGroupButtonClick = (value: string) => {
    if (routeParams?.id != undefined) {
      setActiveButton(value);
    } else {
      setActiveButton('PGS');
    }
  };
  const [selectedItem, setSelectedItem] = useState<number>(
    projectMenuID != null ? projectMenuID : 1
  );
  const menuItems = [
    {
      id: 1,
      name: 'Dashboard',
    },
    {
      id: 2,
      name: 'Project Details',
    },
    {
      id: 3,
      name: 'Member',
    },

    {
      id: 4,
      name: 'BOQ Type',
    },
    {
      id: 5,
      name: 'Sites',
    },
    {
      id: 6,
      name: 'Site Claims',
    },
    {
      id: 7,
      name: 'BOQ',
    },
    {
      id: 8,
      name: 'Documents',
    },
    {
      id: 9,
      name: 'Indents',
    },
    {
      id: 10,
      name: 'Stock Audit',
    },
    {
      id: 11,
      name: 'Stock Outward',
    },
    {
      id: 12,
      name: 'My Orders'
    },
    {
      id: 13,
      name: 'Store'
    },
  ];
  const handleMenuItemClick = (id: number) => {
    dispatch(setToken({ key: 'projectMenuID', value: id }));
    setSelectedItem(id);
  };
  const mainContentComponents: { [key: number]: JSX.Element } = {
    1: (
      <ProjectDashboard
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    2: (
      <ProjectGeneralDetails
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    3: (
      <ProjectSettings
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    4: (
      <ProjectMasterData
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    5: (
      <ProjectSiteConfig
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    6: (
      <ProjectSiteExpenseList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    7: (
      <ProjectBomConfig
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    8: (
      <ProjectDocument
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    9: (
      <ProjectIndentRequestList
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    10: (
      <ProjectStockmanagement
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    11: (
      <ProjectStockOutward
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    12: (
      <MyOrders
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    ),
    13: (
      <Store
        setActiveButton={setActiveButton}
        setLoader={setLoader}
        loader={loader}
      />
    )
  };
  return (
    <CustomLoader loading={loader} size={48}>
      <div className={Styles.Container}>
        <div className={Styles.sub_header}>
          <div
            className={Styles.logo}
            onClick={() => {
              navigate('/project-list');
              dispatch(setToken({ key: 'projectMenuID', value: null }));
            }}
          >
            <PreviousPageIcon width={15} height={15} color="#7f56d9" />
          </div>
          <div style={{ padding: '8px', display: 'flex' }}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div style={{ display: 'flex', alignItems: 'center' }}>
            <div className={Styles.textContent_1}>
              <h3>{projectData?.data?.project_name}</h3>
              <span className={Styles.content}>
                {projectData?.data?.description}
              </span>
            </div>
          </div>
        </div>
        <div className={Styles.selected}></div>
        <div className={Styles.mainContainer}>
          <div className={Styles.sidnav}>
            <SideNav
              menuItems={menuItems}
              selectedItem={selectedItem}
              handleMenuItemClick={handleMenuItemClick}
            />
          </div>
          <div className={Styles.mainbar}>
            {mainContentComponents[selectedItem]}
          </div>
        </div>
      </div>
    </CustomLoader>
  );
};

export default Project;
