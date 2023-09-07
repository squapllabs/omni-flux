import React, { useState } from 'react'
import Styles from '../../styles/labourList.module.scss';
import { useFormik } from 'formik';
import Input from '../ui/Input';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import EditIcon from '../menu/icons/editIcon';
import SearchIcon from '../menu/icons/search';
import { useNavigate } from 'react-router-dom';
import CustomGroupButton from '../ui/CustomGroupButton';

const LabourList = () => {

    const [initialValues, setInitialValues] = useState({
        labour_id: '',
        labour_type: '',
        uom_id: '',
        rate: '',
    });
    const navigate = useNavigate();
    const [activeButton, setActiveButton] = useState<string | null>('AC');
    const [buttonLabels, setButtonLabels] = useState([
        { label: 'Active', value: 'AC' },
        { label: 'Inactive', value: 'IN' },
      ]);

    //   const validationSchema = getCreateValidateyup(Yup);
    const formik = useFormik({
        initialValues,
        // validationSchema,
        enableReinitialize: true,
        onSubmit: (values, { resetForm }) => {
            if (values) {
                console.log("values", values);
            }
        }
    });

    return (
        <div>
            {/* <CustomLoader loading={FilterLoading} size={48} color="#333C44"> */}
            <div>
                <div className={Styles.top}>
                    <div className={Styles.textContent}>
                        <h3>Add New Labour</h3>
                    </div>
                    <div>
                        <Button
                            color="primary"
                            shape="rectangle"
                            justify="center"
                            size="small"
                            icon={<AddIcon />}
                            onClick={() => {
                                  navigate('/labour-add');
                            }}
                        >
                            Add Labour
                        </Button>
                    </div>
                </div>
                <div className={Styles.dividerStyle}></div>
                <div className={Styles.box}>
                    <div className={Styles.textContent}>
                        <h3>List of Labour Data</h3>
                        <span className={Styles.content}>
                            Manage your labour data across your application
                        </span>
                    </div>
                    <div className={Styles.searchField}>
                        <div className={Styles.inputFilter}>
                            <Input
                                width="260px"
                                prefixIcon={<SearchIcon />}
                                name="search_by_name"
                                //   value={filterValues.search_by_name}
                                //   onChange={(e) => handleFilterChange(e)}
                                placeholder="Search by labour type"
                            />
                            <AutoCompleteSelect
                                name="parent_master_data_id"
                                defaultLabel="Select Parent Name"
                                //   onChange={() => handleDropdownChange}
                                //   value={selectedValue}
                                placeholder="UOM Type"
                                width="260px"
                                onSelect={(value) => {
                                    setSelectedValue(value);
                                    // setIsResetDisabled(false);
                                }}
                            //   optionList={
                            //     dropLoading === true ? [] : getAllmasterDataForDrop
                            //   }
                            />
                            <Button
                                className={Styles.searchButton}
                                shape="rectangle"
                                justify="center"
                                size="small"
                            //   onClick={handleSearch}
                            >
                                Search
                            </Button>
                            <Button
                                className={Styles.resetButton}
                                shape="rectangle"
                                justify="center"
                                size="small"
                            //   disabled={isResetDisabled}
                            //   onClick={handleReset}
                            >
                                Reset
                            </Button>
                        </div>
                        <div>
                  <CustomGroupButton
                    labels={buttonLabels}
                    // onClick={handleGroupButtonClick}
                    activeButton={activeButton}
                  />
                </div>
                    </div>
                    <div className={Styles.tableContainer}>
                        <div>
                            <table>
                                <thead>
                                    <tr>
                                        <th>S No</th>
                                        <th>Labour Type</th>
                                        <th>UOM Type</th>
                                        <th>Rate</th>

                                        {/* {activeButton === 'AC' && <th></th>} */}
                                    </tr>
                                </thead>

                            </table>
                        </div>
                    </div>
                    <div className={Styles.pagination}>
                        {/* <Pagination
                currentPage={currentPage}
                totalPages={
                  dataShow ? getFilterData?.total_page : initialData?.total_page
                }
                totalCount={
                  dataShow
                    ? getFilterData?.total_count
                    : initialData?.total_count
                }
                rowsPerPage={rowsPerPage}
                onPageChange={handlePageChange}
                onRowsPerPageChange={handleRowsPerPageChange}
              /> */}
                    </div>
                </div>
                {/* </CustomLoader> */}
            </div>
        </div>
    )
}

export default LabourList;
