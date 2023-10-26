import React, { useEffect, useState } from 'react';
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

  const [pages, setPages] = useState<any>(currentPage);
  const [pageLimitStartCount, setPageLimitStartCount] = useState<any>(1);
  const [pageLimitEndCount, setPageLimitEndCount] = useState<any>(5);
  const [mode, setMode] = useState<any>();
  const numbers = Array.from({ length: totalPages }, (_, index) => index + 1);


  const handlePageChange = (page: number) => {
    onPageChange(page);
    if (currentPage % 5 === 0) {
      setPageLimitStartCount(pageLimitStartCount + 5);
      setPageLimitEndCount(pageLimitEndCount + 5);
    }
  };
  const handlePageChangeDecrement = (page: number) => {
    onPageChange(page);
    if (currentPage % 5 === 1) {
      setPageLimitStartCount(pageLimitStartCount - 5);
      setPageLimitEndCount(pageLimitEndCount - 5);
    }
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

  useEffect(() => {
    if (mode ==='Prev') {
      onPageChange(pageLimitEndCount);
    } else {
      onPageChange(pageLimitStartCount);
    }  
  }, [pageLimitStartCount,pageLimitEndCount]);


  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  const endingIndex = Math.min(startingIndex + rowsPerPage - 1, totalCount);

  return (
    <div className={Styles.container}>
      <div
        className={Styles.navigateIcon}
        onClick={() => {
          setMode('Prev')
          handlePageChangeDecrement(currentPage - 1)
        }
        }
        style={{ pointerEvents: currentPage === 1 ? 'none' : '' }}
      >
        <FirstPageIcon disabled={currentPage === 1} width={20} height={20} />
        <span style={{ color: '#667085', fontSize: "14px", fontWeight: "400" }}>Previous</span>
      </div>
      <div>
        <div className={Styles.numberCount}>
          <div>
            <PreviousPage
              width={10}
              height={10}
              onClick={() => {
                setMode('Prev')
                setPageLimitStartCount(pageLimitStartCount - 5);
                setPageLimitEndCount(pageLimitEndCount - 5);
              }}
              disabled={pageLimitStartCount <= numbers[0]}
            />
          </div>
          {numbers?.map((value: any, index: any) => {
            if (
              Number(value) >= Number(pageLimitStartCount) &&
              Number(value) <= Number(pageLimitEndCount)
            ) {
              return (
                <div>
                  <div
                    className={
                      currentPage === value
                        ? `${Styles.menu_item} ${Styles.selected}`
                        : `${Styles.menu_items}`
                    }
                    onClick={() => {
                      handleClickChange(value);
                    }}
                  >
                    <div >{value}</div>
                  </div>
                </div>
              );
            }
          })}
          <div
            style={{
              pointerEvents: pageLimitEndCount >= totalPages ? 'none' : '',
            }}
          >
            <NextPage
              width={10}
              height={10}
              onClick={() => {
                setMode('Next')
                setPageLimitStartCount(pageLimitStartCount + 5);
                setPageLimitEndCount(pageLimitEndCount + 5);
              }}
              disabled={pageLimitEndCount >= totalPages}
            />
          </div>
        </div>
      </div>

      <div
        className={Styles.navigateIcon}
        onClick={() => {
          setMode('Next')
          handlePageChange(currentPage + 1)
        }
        }
        style={{ pointerEvents: currentPage === totalPages ? 'none' : '' }}
      >
        <span style={{ color: '#667085', fontSize: "14px", fontWeight: "400" }}>Next</span>
        <LastPageIcon
          disabled={currentPage === totalPages}
          width={20}
          height={20}
        />
      </div>
    </div>
  );
};

export default CustomPagination;
