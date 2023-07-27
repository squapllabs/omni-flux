import React, { useState } from 'react';
import NextPage from '../menu/icons/nextPageIcon';
import PreviousPage from '../menu/icons/previousPageIcon';
import FirstPageIcon from '../menu/icons/firstPageIcon';
import Styles from '../../styles/pagination.module.scss';
import CustomSelect from '../ui/customSelect';
import LastPageIcon from '../menu/icons/lastPageIcon';
const options = [
  { value: '3', label: '3 ' },
  { value: '5', label: '5' },
  { value: '10', label: '10' },
  { value: '20', label: '20' },
];
interface PaginationProps {
  currentPage: number;
  totalPages: number;
  rowsPerPage: number;

  onPageChange: (page: number) => void;
  onRowsPerPageChange: (rowsPerPage: number) => void;
}

const Pagination: React.FC<PaginationProps> = ({
  currentPage,
  totalPages,

  rowsPerPage,
  onPageChange,
  onRowsPerPageChange,
}) => {
  const handlePageChange = (page: number) => {
    onPageChange(page);
  };

  const handleRowsPerPageChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const newRowsPerPage = parseInt(event.target.value, 10);
    onRowsPerPageChange(newRowsPerPage);
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.rowPerPage}>
        <div className={Styles.title}>Rows Per Page</div>
        <CustomSelect
          label=""
          options={options}
          value={rowsPerPage}
          onChange={handleRowsPerPageChange}
          width="70px"
        />
      </div>
      <span className={Styles.pagesCount}>
        {currentPage} - {rowsPerPage}of{totalPages}
      </span>

      <div className={Styles.icons}>
        <FirstPageIcon
          onClick={() => handlePageChange(1)}
          disabled={currentPage === 1}
          width={40}
          height={40}
        />

        <PreviousPage
          onClick={() => handlePageChange(currentPage - 1)}
          disabled={currentPage === 1}
          width={20}
          height={20}
        />

        <NextPage
          onClick={() => handlePageChange(currentPage + 1)}
          disabled={currentPage === totalPages}
          width={20}
          height={20}
        />

        <LastPageIcon
          onClick={() => handlePageChange(totalPages)}
          disabled={currentPage === totalPages}
          width={40}
          height={40}
        />
      </div>
    </div>
  );
};

export default Pagination;
