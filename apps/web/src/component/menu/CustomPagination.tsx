import React, { useState } from 'react';
import NextPage from '../menu/icons/nextPageIcon';
import PreviousPage from '../menu/icons/previousPageIcon';
import FirstPageIcon from '../menu/icons/firstPageIcon';
import Styles from '../../styles/newpagination.module.scss';
import LastPageIcon from '../menu/icons/lastPageIcon';
import Select from '../ui/selectNew';
const options = [
  { value: '5', label: 5 },
  { value: '10', label: 10 },
  { value: '20', label: 20 },
];
interface PaginationProps {
  currentPage: number;
  totalPages: number;
  rowsPerPage: number;
  totalCount: number;
  onPageChange: (page: number) => void;
  onRowsPerPageChange: (rowsPerPage: number) => void;
}

const CustomPagination: React.FC<PaginationProps> = ({
  currentPage,
  totalPages,
  rowsPerPage,
  totalCount,
  onPageChange,
  onRowsPerPageChange,
}) => {
  console.log('currentPage', currentPage);

  const [pages, setPages] = useState<any>(currentPage);
  const numbers = Array.from({ length: totalPages }, (_, index) => index + 1);
  console.log('numbers', numbers);

  const handlePageChange = (page: number) => {
    onPageChange(page);
  };
  const handleClickChange = (page: number) => {
    onPageChange(page);
  };
  const handleRowsPerPageChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const newRowsPerPage = parseInt(event.target.value, 10);
    onRowsPerPageChange(newRowsPerPage);
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  const endingIndex = Math.min(startingIndex + rowsPerPage - 1, totalCount);

  return (
    <div className={Styles.container}>
      <div
        className={Styles.navigateIcon}
        onClick={() => handlePageChange(currentPage - 1)}
        style={{ pointerEvents: currentPage === 1 ? 'none' : '' }}
      >
        <PreviousPage disabled={currentPage === 1} width={10} height={10} />
        <span>Previous</span>
      </div>
      <div>
        <div className={Styles.numberCount}>
          {numbers?.map((value: any, index: any) => {
            return (
              <div>
                <div
                  className={
                    currentPage === value
                      ? `${Styles.menu_item} ${Styles.selected}`
                      : `${Styles.menu_item}`
                  }
                  onClick={() => {
                    handleClickChange(value);
                  }}
                >
                  {value}
                </div>
              </div>
            );
          })}
        </div>
      </div>

      <div
        className={Styles.navigateIcon}
        onClick={() => handlePageChange(currentPage + 1)}
        style={{ pointerEvents: currentPage === totalPages ? 'none' : '' }}
      >
        <span>Next</span>
        <NextPage
          disabled={currentPage === totalPages}
          width={10}
          height={10}
        />
      </div>
    </div>
  );
};

export default CustomPagination;
