import { createSlice } from '@reduxjs/toolkit';
import { RootState } from './store';

export const authSlice = createSlice({
  name: 'auth',
  initialState: {
    key: null,
    value: null,
  },
  reducers: {
    setToken: (state, action) => {
      const { key, value } = action.payload;
      state.key = key;
      state.value = value;
    },
  },
});

export const { setToken } = authSlice.actions;

export const getToken = (state: RootState, key: string) => {
  const { value } = state.auth;
  if (state.auth.key !== null && state.auth.key === key) {
    return value;
  }
  return null;
};

export default authSlice.reducer;
