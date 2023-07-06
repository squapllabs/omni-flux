// import { configureStore } from '@reduxjs/toolkit';
// import authReducer from './reducer';

// export default configureStore({
//   reducer: {
//     auth: authReducer,
//   },
// });
import { configureStore } from '@reduxjs/toolkit';
import { combineReducers } from 'redux';
import authReducer from './reducer';

const rootReducer = combineReducers({
  auth: authReducer,
});

export type RootState = ReturnType<typeof rootReducer>;

export default configureStore({
  reducer: rootReducer,
});
