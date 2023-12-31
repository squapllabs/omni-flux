import React from 'react';
import NextPage from '../menu/icons/nextPageIcon';
import PreviousPage from '../menu/icons/previousPageIcon';
import FirstPageIcon from '../menu/icons/firstPageIcon';
import Styles from '../../styles/pagination.module.scss';
import LastPageIcon from '../menu/icons/lastPageIcon';
import Select from '../ui/selectNew';
const options = [
  { value: '5', label: '5' },
  { value: '10', label: '10' },
  { value: '20', label: '20' },
  { value: '25', label: '25' },
];
interface PaginationProps {
  currentPage: number;
  totalPages: number;
  rowsPerPage: number;
  numberOfRecords: number;
  onPageChange: (page: number) => void;
  onRowsPerPageChange: (rowsPerPage: number) => void;
}

const Pagination: React.FC<PaginationProps> = ({
  currentPage,
  totalPages,
  rowsPerPage,
  numberOfRecords,
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
        <div className={Styles.rowsNo}>
          <Select
            label=""
            value={rowsPerPage}
            onChange={handleRowsPerPageChange}
          >
            {options.map((option: any) => (
              <option key={options.value} value={options.value}>
                {option.label}
              </option>
            ))}
          </Select>
        </div>
      </div>
      <span className={Styles.pagesCount}>
        <span style={{ paddingRight: '5px' }}>
          {currentPage}-{(currentPage - 1) * rowsPerPage}
        </span>
        of
        <span style={{ paddingLeft: '5px' }}> {numberOfRecords}</span>
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
