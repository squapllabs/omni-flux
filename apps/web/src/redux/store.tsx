import { configureStore, getDefaultMiddleware } from '@reduxjs/toolkit';
import { combineReducers } from 'redux';
import { persistStore, persistReducer } from 'redux-persist';
import storage from 'redux-persist/lib/storage';
import authReducer from './reducer';
import { ThunkDispatch } from 'redux-thunk';
import { Action } from 'redux';

const rootReducer = combineReducers({
  auth: authReducer,
});

const persistConfig = {
  key: 'root',
  storage,
};

const persistedReducer = persistReducer(persistConfig, rootReducer);

// type ThunkResult<R> = ThunkAction<R, RootState, undefined, Action<string>>;
type ThunkDispatchType = ThunkDispatch<RootState, undefined, Action<string>>;

const middleware = [
  ...getDefaultMiddleware<RootState, ThunkDispatchType>({
    serializableCheck: {
      ignoredActions: ['persist/PERSIST'],
      ignoredActionPaths: [['register']],
    },
  }),
];

export const store = configureStore({
  reducer: persistedReducer,
  middleware,
});
export type RootState = ReturnType<typeof persistedReducer>;
export const persistor = persistStore(store);
