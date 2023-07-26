import React from 'react';

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
    <div>
      <select value={rowsPerPage} onChange={handleRowsPerPageChange}>
        <option value={5}>5 Rows Per Page</option>
        <option value={10}>10 Rows Per Page</option>
        <option value={20}>20 Rows Per Page</option>
      </select>
      <button onClick={() => handlePageChange(1)} disabled={currentPage === 1}>
        First Page
      </button>
      <button
        onClick={() => handlePageChange(currentPage - 1)}
        disabled={currentPage === 1}
      >
        Previous Page
      </button>

      {/* <span>
        Page {currentPage} of {totalPages}
      </span> */}

      <button
        onClick={() => handlePageChange(currentPage + 1)}
        disabled={currentPage === totalPages}
      >
        Next Page
      </button>
      <button
        onClick={() => handlePageChange(totalPages)}
        disabled={currentPage === totalPages}
      >
        Last Page
      </button>
    </div>
  );
};

export default Pagination;
