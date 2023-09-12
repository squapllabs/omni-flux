import catchAsync from '../utils/catchAsync';
import * as projectSiteService from '../services/projectSite.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const getByProjectId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectId';
  try {
    const projectSite = await projectSiteService.getByProjectId(
      req.params.project_id
    );
    res.send(projectSite);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { getByProjectId };
