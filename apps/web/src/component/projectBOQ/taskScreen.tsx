import React, { useEffect, useState} from 'react';
import Styles from '../../styles/newStyles/projectAbstract.module.scss'
import PreviousPageIcon  from '../menu/icons/previousPageIcon';
import { useNavigate, useParams } from 'react-router-dom';
import categoryService from '../../service/category-service';
import { formatBudgetValue } from '../../helper/common-function';
import ClipboardIcon from '../menu/icons/clipboardIcon';
import BomItems from '../projectBOQ/boqItems';
import { useGetByBOMDetails } from '../../hooks/category-hooks';
import CategoryService from '../../service/category-service';

const BoqTaskListScreen : React.FC= ()=>{

    const navigate = useNavigate();
    const routeParams = useParams();

    const routeParamsObj = {
        projectId : Number(routeParams?.projectId),
        boQId : Number(routeParams.bomconfigId),
        categoryId : Number(routeParams.categoryId)
    }    
    const  projectId = Number(routeParams?.projectId);
    const  bomConfigId = Number(routeParams.bomconfigId);
    const [selectedCategory, setSelectedCategory] = useState();
    // const [categories, setCategories] = useState();
    // const [isloading, setIsloading] = useState(true);
    const [categoryData, setCategoryData] = useState();
    // const [categoryId, setCategoryId] = useState();
    const [selectedSubCategory, setSelectedSubCategory] = useState();
    const [reload, setReload] = useState(false);


    const { data: getBomData } = useGetByBOMDetails(routeParamsObj);

    useEffect(() => {
      const fetchData = async () => {
        const obj = {
          projectId: projectId,
          bomconfigId: bomConfigId,
        };

        const categoryData = await CategoryService.getOneCategoryByID(routeParamsObj.categoryId);
        // setCategories(datas.data);
        // setIsloading(false);
        setCategoryData(categoryData.data);
        // setSelectedCategory(datas.data[0].category_id);
        // setCategoryId(datas.data[0].category_id);
        
      };
  
      fetchData();
    }, [reload]);
    return (
        <div>
      <div className={Styles.container}>
        <div className={Styles.sub_header}>
          <div style={{ display: 'flex' }}>
            <div
              className={Styles.logo}
              onClick={() => {
                navigate(`/newBoq/${routeParams?.projectId}/${routeParams?.bomconfigId}`);
              }}
            >
              <PreviousPageIcon width={15} height={15} color="#7f56d9" />
              <span>Back to Abstract List</span>
            </div>
            <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div style={{ display: 'flex', alignItems: 'center', gap: '20px' }}>
              <div>
                <ClipboardIcon width={30} height={30} />
              </div>
              <div className={Styles.textContent_1}>
                <span className={Styles.projectTitle}>
                  {
                    getBomData?.bom_configuration_data?.project_data
                      ?.project_name
                  }
                </span>
                {/* <span className={Styles.content}>
                  {getBomData?.bom_configuration_data?.bom_name}
                </span> */}
              </div>
            </div>
            <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div className={Styles.countContent}>
              <h3>{getBomData?.bom_configuration_data?.bom_description}</h3>
              
            </div>
          </div>

          <div className={Styles.boqAmount}>
            <div className={Styles.lineStyles}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            {/* <div className={Styles.countContent}>
              <h3>
                {formatBudgetValue(
                  getBomData?.bom_configuration_data?.budget
                    ? getBomData?.bom_configuration_data?.budget
                    : 0
                )}
              </h3>
              <span className={Styles.countContentTitle}>Aggregated Value</span>
            </div> */}
          </div>
        </div>
        <div className={Styles.selected}></div>
        <div>
          <BomItems
            selectedCategory={categoryData?categoryData:''}
            setSelectedSubCategory={setSelectedSubCategory}
            selectedSubCategory={selectedSubCategory}
            projectsId={projectId}
            selectedBomConfig={bomConfigId}
                  />
        </div>
      </div>
    </div>
    );
};

export default BoqTaskListScreen;