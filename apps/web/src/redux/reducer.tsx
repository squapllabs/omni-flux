import { createSlice } from '@reduxjs/toolkit';
import { RootState } from './store';

interface AuthState {
  [key: string]: any;
}

export const authSlice = createSlice({
  name: 'auth',
  initialState: {} as AuthState,
  reducers: {
    setToken: (state, action) => {
      const { key, value } = action.payload;
      state[key] = value;
    },
    resetAuth: () => {
      return {} as AuthState;
    },
  },
});

export const { setToken, resetAuth } = authSlice.actions;

export const getToken = (state: RootState, key: string) => {
  return state.auth[key] || null;
};

export default authSlice.reducer;
