import React from 'react';
import Styles from '../../../../styles/project.module.scss';
import { getProjectBasedIndent } from '../../../../hooks/indentRequest-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import EditIcon from '../../../menu/icons/editIcon';
import { format } from 'date-fns';
import Button from '../../../ui/Button';
import AddIcon from '../../../menu/icons/addIcon';
import { formatBudgetValue } from '../../../../helper/common-function';
import Select from '../../../ui/selectNew';
const ProjectIndentRequestList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const priority: any = [
    { value: 'High', label: 'High' },
    { value: 'Medium', label: 'Medium' },
    { value: 'Low', label: 'Low' },
  ];
  const { data: getIndentList } = getProjectBasedIndent(
    Number(routeParams?.id)
  );
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'dd-MM-yyyy');
    return formattedDate;
  };
  return (
    <div>
      <div className={Styles.headingContent}>
        <div className={Styles.textContent_1}>
          <h3>Indent Request</h3>
          <span className={Styles.content}>Add Indent Request</span>
        </div>
        <div>
          <Button
            type="button"
            color="primary"
            shape="rectangle"
            size="small"
            justify="center"
            icon={<AddIcon width={20} color="white" />}
            onClick={(e) => {
              navigate(`/indent/${routeParams?.id}`);
            }}
          >
            Add
          </Button>
        </div>
      </div>
      <div className={Styles.searchField}>
        <div className={Styles.inputFilter}>
          <Select
            label="Priority"
            name="priority"
            mandatory={true}
            // onChange={formik.handleChange}
            // value={formik.values.priority}
            defaultLabel="Select from options"
            placeholder="Select from options"
            // error={
            //   formik.touched.priority && formik.errors.priority
            // }
            // disabled={disabled}
          >
            {priority?.map((items: any, index: any) => {
              return (
                <option key={items.value} value={items.value}>
                  {items.label}
                </option>
              );
            })}
          </Select>
          <Button
            className={Styles.searchButton}
            type="button"
            color="primary"
            shape="rectangle"
            size="small"
            justify="center"
          >
            Search
          </Button>
          <Button
            className={Styles.resetButton}
            type="button"
            color="secondary"
            shape="rectangle"
            size="small"
            justify="center"
          >
            Reset
          </Button>
        </div>
      </div>
      <div className={Styles.tableContainer}>
        <div>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>S No</th>
                <th className={Styles.tableHeadingSite}>
                  Indent Requested Date
                </th>
                <th className={Styles.tableHeading}>Expected Delivery Date</th>
                <th className={Styles.tableHeading}>Cost</th>
                <th className={Styles.tableHeading}>Indent Status</th>
                <th className={Styles.tableHeading}>Action</th>
              </tr>
            </thead>
            <tbody>
              {getIndentList?.map((items: any, index: any) => {
                rowIndex = rowIndex + 1;
                return (
                  <tr>
                    <td>{rowIndex}</td>
                    <td>{dateFormat(items?.requested_date)}</td>
                    <td>{dateFormat(items?.expected_delivery_date)}</td>
                    <td>{formatBudgetValue(items?.total_cost)}</td>
                    <td>{items?.approver_status}</td>
                    <td>
                      <div
                        style={{
                          cursor: 'pointer',
                        }}
                      >
                        <EditIcon
                          onClick={(e) => {
                            navigate(
                              `/indent/${routeParams?.id}/${items?.indent_request_id}`
                            );
                          }}
                        />
                      </div>
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
};

export default ProjectIndentRequestList;
