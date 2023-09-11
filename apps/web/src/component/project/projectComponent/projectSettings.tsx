import React, { useState } from 'react'
import Styles from '../../../styles/projectSettings.module.scss'
import Input from '../../ui/Input';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import AddIcon from '../../menu/icons/addIcon';
import SearchIcon from '../../menu/icons/search';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import DatePicker from '../../ui/CustomDatePicker';
import CustomGroupButton from '../../ui/CustomGroupButton';
import { useParams, useNavigate } from 'react-router-dom';


const ProjectSettings : React.FC = (props: any) => {
    
    const [buttonLabels, setButtonLabels] = useState([
        { label: 'Active', value: 'AC' },
        { label: 'Inactive', value: 'IN' },
    ]);
    const [activeButton, setActiveButton] = useState<string | null>('AC');
    const routeParams = useParams();
    console.log('routeParams', routeParams?.id);

    const formik = useFormik({
        initialValues: {
            //   email: '',
        },
        onSubmit: values => {
            alert(JSON.stringify(values, null, 2));
        },
    });

    return (

        <div className={Styles.conatiner}>
            <div className={Styles.box}>
                <div className={Styles.textContent}>
                    <h3>Invite Member</h3>
                    {/* <span className={Styles.content}>
                        OMNI Flux Members *
                    </span> */}
                </div>
                <form onSubmit={formik.handleSubmit}>
                    <div className={Styles.fields_container}>
                        <div className={Styles.fields_container_1}>
                            <div>
                                <AutoCompleteSelect
                                    label="OMNI Flux Members"
                                    name="parent_master_data_id"
                                    onChange={formik.handleChange}
                                    // value={formik.values.parent_master_data_id}
                                    placeholder="Select from options"
                                    mandatory
                                    width="350px"
                                    onSelect={(value) => {
                                        formik.setFieldValue('parent_master_data_id', value);
                                    }}
                                // optionList={
                                //     dropLoading === true ? [] : getAllmasterDataForDrop
                                // }
                                // error={
                                //     formik.touched.parent_master_data_id &&
                                //     formik.errors.parent_master_data_id
                                // }
                                />
                            </div>


                            <div>
                                <AutoCompleteSelect
                                    label="Permission Role"
                                    name="parent_master_data_id"
                                    onChange={formik.handleChange}
                                    // value={formik.values.parent_master_data_id}
                                    placeholder="Select from options"
                                    width="350px"
                                    mandatory
                                    onSelect={(value) => {
                                        formik.setFieldValue('parent_master_data_id', value);
                                    }}
                                // optionList={
                                //     dropLoading === true ? [] : getAllmasterDataForDrop
                                // }
                                // error={
                                //     formik.touched.parent_master_data_id &&
                                //     formik.errors.parent_master_data_id
                                // }
                                />
                            </div>
                        </div>

                        <div className={Styles.fields_container_2}>
                            <div >
                                <DatePicker
                                    label="Access Start Date"
                                    name="start_date"
                                    onChange={formik.handleChange}
                                    width='350px'
                                //   value={formik.values.start_date}
                                // mandatory
                                //   error={formik.touched.start_date && formik.errors.start_date}
                                />
                            </div>
                            <div>
                                <DatePicker
                                    label="Access Expiration Date"
                                    name="start_date"
                                    onChange={formik.handleChange}
                                    width='350px'
                                //   value={formik.values.start_date}
                                // mandatory
                                //   error={formik.touched.start_date && formik.errors.start_date}
                                />
                            </div>
                            <div className={Styles.inputField}>

                                <div>
                                    <Button
                                        color="primary"
                                        shape="rectangle"
                                        justify="center"
                                        size="small"
                                        icon={<AddIcon />}
                                    >
                                        Add
                                    </Button>
                                </div>
                            </div>
                        </div>
                    </div>
                </form>
            </div>
            <div className={Styles.box}>
                <div className={Styles.textContent}>
                    <h3>Existing Members *</h3>
                    <span className={Styles.content}>
                        Members of Sample Project
                    </span>
                </div>
                <div className={Styles.searchField}>
                    <div className={Styles.inputFilter}>

                        <AutoCompleteSelect
                            name="parent_master_data_id"
                            defaultLabel="Select Parent Name"
                            //   onChange={() => handleDropdownChange}
                            //   value={selectedValue}
                            placeholder="User Name"
                            width="260px"
                            onSelect={(value) => {
                                // setSelectedValue(value);
                                // setIsResetDisabled(false);
                            }}
                        //   optionList={
                        // dropLoading === true ? [] : getAllmasterDataForDrop
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
                            //   onClick={handleGroupButtonClick}
                            activeButton={activeButton}
                        />
                    </div>
                </div>
                <div className={Styles.tableContainer}>
              <div>
                <table>
                  <thead>
                    <tr>
                    <th>S NO</th>
                      <th>Name</th>
                      <th>Role</th>
                      <th>Expiration Date</th>
                      {activeButton === 'AC' && <th></th>}
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                        <td>
                            1
                        </td>
                        <td>
                            Velavan
                        </td>
                        <td>
                            Site Engineer
                        </td>
                        <td>
                            11-10-2023
                        </td>
                    </tr>
                  </tbody>
                  {/* <tbody>
                    {dataShow ? (
                      getFilterData?.total_count === 0 ? (
                        <tr>
                          <td></td>
                          <td></td>
                          <td>No data found</td>
                          {activeButton === 'AC' && <td></td>}
                        </tr>
                      ) : (
                        getFilterData?.content?.map(
                          (data: any, index: number) => (
                            <tr key={data.hsn_code_id}>
                          <td>{startingIndex + index}</td>
                              <td>{data.code}</td>
                              <td>
                                <span
                                  className={Styles.truncatedStyle}
                                  title={data.description}
                                >
                                  {data.description?data.description.substring(0, 30): '-'}
                                </span>
                              </td>
                              {activeButton === 'AC' && (
                                <td>
                                  <div className={Styles.tablerow}>
                                    <EditIcon
                                      onClick={() =>
                                        editHscCodeHandler(data.hsn_code_id)
                                      }
                                    />
                                    <DeleteIcon
                                      onClick={() =>
                                        deleteCategoryHandler(data.hsn_code_id)
                                      }
                                    />
                                  </div>
                                </td>
                              )}
                            </tr>
                          )
                        )
                      )
                    ) : initialData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td></td>
                        <td>No data found</td>
                        {activeButton === 'AC' && <td></td>}
                      </tr>
                    ) : (
                      initialData?.content?.map((data: any, index: number) => (
                        <tr key={data.hsn_code_id}>
                          <td>{startingIndex + index}</td>
                          <td>{data.code}</td>
                          <td>
                            <span
                              title={data.description}
                            >
                              {data.description?data.description.substring(0, 30): '-'}
                            </span>
                          </td>
                          {activeButton === 'AC' && (
                            <td>
                              <div className={Styles.tablerow}>
                                <EditIcon
                                  onClick={() =>
                                    editHscCodeHandler(data.hsn_code_id)
                                  }
                                />
                                <DeleteIcon
                                  onClick={() =>
                                    deleteCategoryHandler(data.hsn_code_id)
                                  }
                                />
                              </div>
                            </td>
                          )}
                        </tr>
                      ))
                    )}
                  </tbody> */}
                </table>
              </div>
            </div>

            </div>
        </div>
    )
}

export default ProjectSettings
