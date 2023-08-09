import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectlist.module.scss';
import { useGetAllProject, getByProject } from '../../hooks/project-hooks';
import Input from '../ui/Input';
import Button from '../ui/Button';
import SearchIcon from '../menu/icons/search';
import AddIcon from '../menu/icons/addIcon';
import CustomLoader from '../ui/customLoader';
import CustomGroupButton from '../ui/CustomGroupButton';
const ProjectList = () => {
//   const { isLoading: getAllLoading } = useGetAllProject;
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    // isLoading: FilterLoading,
  } = getByProject();
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [filter, setFilter] = useState(false);
  const [isLoading, setIsLoading] = useState(true);

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
  };
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  useEffect(() => {
    handleSearch();
  }, [activeButton]);

  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const userData: any = {
      limit: 5,
      offset: 0,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      global_search: filterValues.search_by_name,
      status: activeButton,
    };
    postDataForFilter(userData);
    setIsLoading(false);
    setFilter(true);
  };

  /* Function for reseting the table to its actual state after search */
  const handleReset = async () => {
    const userData: any = {
      limit: 5,
      offset: 0,
      order_by_column: 'updated_by',
      order_by_direction: 'desc',
      global_search: '',
      status: 'AC',
    };
    postDataForFilter(userData);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
  };

  return (
    <div className={Styles.container}>
      <div>
        <div className={Styles.text}>
          <div className={Styles.textStyle}>
            <h3>List of Projects</h3>
          </div>
          <div className={Styles.textStyle}>
            <h6>Project List</h6>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.searchField}>
          <div className={Styles.inputFilter}>
            <Input
              width="260px"
              prefixIcon={<SearchIcon />}
              name="search_by_name"
              value={filterValues.search_by_name}
              onChange={(e) => handleFilterChange(e)}
              placeholder="Search"
            />
            <Button
              className={Styles.searchButton}
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleSearch}
            >
              Search
            </Button>
            <Button
              className={Styles.resetButton}
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleReset}
            >
              Reset
            </Button>
          </div>
          <div>
            <div>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
            </div>
            <Button
              shape="rectangle"
              justify="center"
              size="small"
              color="primary"
              icon={<AddIcon />}
            >
              Add
            </Button>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S. No</th>
                  <th>Name</th>
                  <th>Code</th>
                  <th>Manager</th>
                  <th>Status</th>
                  <th>Start Date</th>
                  <th>End Date</th>
                  <th></th>
                </tr>
              </thead>
              <tbody>
                {getFilterData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                    <td></td>
                  </tr>
                ) : (
                  ''
                )}
                {getFilterData?.content?.map((data: any, index: number) => (
                  <tr key={data.user_id}>
                    <td>{index + 1}</td>
                    <td>{data.project_name}</td>
                    <td>{data.code}</td>
                    <td>
                      {data.user?.first_name} {data.user?.last_name}
                    </td>
                    <td>{data.status}</td>
                    <td>{data.date_started}</td>
                    <td>{data.date_ended}</td>
                    <td>
                      {/* <div className={Styles.tablerow}>
                          <EditIcon onClick={() => navigate(`/user-edit/${data.user_id}`)} />
                          <DeleteIcon onClick={() => deleteUserHandler(data.user_id)} />
                        </div> */}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ProjectList;
