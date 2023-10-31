import ProjectSubheader from '../../../project/projectSubheader';
import Styles from '../../../../styles/viewReceivedGoods.module.scss'
import { useNavigate, useParams, useLocation } from 'react-router-dom';
import PreviousPageIcon from '../../../menu/icons/previousPageIcon';



const ViewReceivedGoods = () => {
    const routeParams = useParams();
    const navigate = useNavigate();
    const purchaseOrderId = Number(routeParams?.id)
    const { state } = useLocation();
    const projectId = state?.projectId;
    console.log("State", state);

    // const projectId = 
    return (
        <div className={Styles.container}>
            <div className={Styles.sub_header}>
                <div
                    className={Styles.logo}
                    onClick={() => {
                        navigate(`/my-orders-view/${Number(purchaseOrderId)}`,
                        {state: {projectId}});
                    }}
                >
                    <PreviousPageIcon width={20} height={20} color="#7f56d9" />
                </div>
                <div style={{ display: 'flex' }}>
                    <div className={Styles.vertical}>
                        <div className={Styles.verticalLine}></div>
                    </div>
                </div>
                <div className={Styles.orderDetails}>
                    <div className={Styles.leftOrderDetail}>
                        <span>
                            <h4>Received Goods</h4>
                        </span>
                        <span>
                            <p className={Styles.description}>View the Received Goods details</p>
                        </span>
                    </div>
                </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            {/* <div>
                <ProjectSubheader
                    
                    navigation={`/my-orders-view/${purchaseOrderId}`}
                    title="Received Goods"
                    description="View the Received Goods details"
                />
            </div> */}
            <div>
                <div className={Styles.tableContainer}>
                    <table className={Styles.scrollable_table}>
                        <thead>
                            <tr>
                                <th>S No</th>
                                <th>Total Items</th>
                                <th>Received Quantity</th>
                                <th>Received Date</th>
                                <th>Options</th>
                            </tr>
                        </thead>
                        <tbody>

                        </tbody>
                    </table>
                </div>
            </div>
        </div>
    )
}

export default ViewReceivedGoods;