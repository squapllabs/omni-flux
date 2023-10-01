import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/projectMasterDataAdd.module.scss';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import { useParams } from 'react-router-dom';
import MasterDataIcon from '../../menu/icons/masterDataIcon';
import { useGetAllPaginatedMasterData } from '../../../hooks/masertData-hook';
import EditIcon from '../../menu/icons/newEditIcon';
import CustomLoader from '../../ui/customLoader';
import CustomGroupButton from '../../ui/CustomGroupButton';
import CustomPagination from '../../menu/CustomPagination';
import CustomPopup from '../../ui/CustomRightSidePopup';
import ProjectMasterDataEditForm from './projectMasterDataEdit';
import CustomSnackBar from '../../ui/customSnackBar';
import DeleteIcon from '../../menu/icons/newDeleteIcon';

const ProjectMasterData: React.FC = (props: any) => {
  const routeParams = useParams();
  const projectId = Number(routeParams?.id);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [mode, setMode] = useState('');
  const [masterId,setMasterId] = useState();
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const masterData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: activeButton,
    global_search: '',
    project_id: Number(routeParams?.id),
    parent_id: null,
    project_master_data: false,
  };
  const {
    data: initialData,
    refetch,
    isLoading: getAllLoadingProjectMasterData,
  } = useGetAllPaginatedMasterData(masterData);
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleAddMasterData = () => {
    setOpen(true);
    setMode('ADD')
  }
  const handleEdit = (value: any) => {
    setMasterId(value)
    setOpen(true);
    setMode('EDIT')
  }
  
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);
  return (
    <div className={Styles.conatiner}>
    <div>
      <CustomLoader
        loading={getAllLoadingProjectMasterData}
        size={48}
        color="#333C44"
      >
        {/* Header Part */}
        {initialData?.total_count !== 0 || activeButton === 'IN' ? (
          <div>
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                <div className={Styles.subHeading}>
                  <MasterDataIcon />
                  <h4>MASTER DATA</h4>
                </div>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon color="white" />}
                    onClick={handleAddMasterData}
                  >
                    Add Master Data
                  </Button>
                </div>
              </div>
              <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div>
            </div>
            {/* Table Part */}
            <div className={Styles.tableContainer}>
              <div>
                <table className={Styles.scrollable_table}>
                  <thead>
                    <tr>
                      <th className={Styles.tableHeading}>#</th>
                      <th className={Styles.tableHeading}>Name</th>
                      <th className={Styles.tableHeading}>Description</th>
                      <th className={Styles.tableHeading}>Code</th>
                      <th className={Styles.tableHeading}>Action</th>
                    </tr>
                  </thead>
                  <tbody>
                    {initialData?.total_count === 0 ? (
                      <tr>
                        <td colSpan="5" style={{ textAlign: 'center' }}>
                          No data found
                        </td>
                      </tr>
                    ) : (
                      initialData?.content?.map((data: any, index: number) => {
                        return (
                          <tr key={data?.master_data_id}>
                            <td>{startingIndex + index}</td>
                            <td>{data?.master_data_name}</td>
                            <td>{data?.master_data_description}</td>
                            <td>{data?.master_data_type}</td>
                            <td>
                                <div className={Styles.iconStyle}>
                              <EditIcon onClick={() => handleEdit(data?.master_data_id)}/>
                              <DeleteIcon/>
                              </div>
                            </td>
                          </tr>
                        );
                      })
                    )}
                  </tbody>
                </table>
              </div>
              <div>
                <CustomPagination
                  currentPage={currentPage}
                  totalPages={initialData?.total_page}
                  totalCount={initialData?.total_count}
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        ) : (
          <div>
            <div className={Styles.subHeading}>
              <MasterDataIcon />
              <span>MASTER DATA</span>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div>
                <img
                  src="/masterDataImage.png"
                  alt="aa"
                  width="75%"
                  height="75%"
                />
              </div>
              <div>
                <h5>This project has no Master Data</h5>
              </div>
              <div>
                <span className={Styles.spanContent}>Go ahead, add a Master Data to this project now</span>
              </div>
              <div className={Styles.emptyButton}>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => setOpen(true)}
                >
                  Add Master Data
                </Button>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
      <CustomPopup
        open={open}
        content={
          <ProjectMasterDataEditForm
            setOpen={setOpen}
            open={open}
            mode={mode}
            projectId={projectId}
            masterID={masterId}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
          />
        }
      />
    </div>
    </div>
  );
};

export default ProjectMasterData;
