import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/projectMasterDataAdd.module.scss';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import { useNavigate, useParams } from 'react-router-dom';
import MasterDataIcon from '../../menu/icons/masterDataIcon';
import {
  useGetAllmasertData,
  createmasertData,
  useGetAllPaginatedMasterData,
  useDeletemasertData,
} from '../../../hooks/masertData-hook';
import CustomGroupButton from '../../ui/CustomGroupButton';
import CustomPagination from '../../menu/CustomPagination';

const ProjectMasterData: React.FC = (props: any) => {
    const routeParams = useParams();
  
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
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
  const { data: initialData, refetch } =
    useGetAllPaginatedMasterData(masterData);
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  console.log('zzzzzzzzzzzzzzzzzz', initialData);

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
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
    <div>
      {/* Header Part */}
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
            >
              Add To Project
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
              </tr>
            </thead>
            <tbody>
              {initialData?.content?.map((data: any, index: number) => {
                return (
                  <tr key={data?.master_data_id}>
                    <td>{startingIndex + index}</td>
                    <td>{data?.master_data_name}</td>
                    <td>{data?.master_data_description}</td>
                    <td>{data?.master_data_type}</td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
        <div className={Styles.pagination}>
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
  );
};

export default ProjectMasterData;
