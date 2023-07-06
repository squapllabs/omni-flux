import { createSlice } from '@reduxjs/toolkit';

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
      console.log('Value updated:', value);
    },
  },
});

export const { setToken } = authSlice.actions;

export const getToken = (
  state: { auth: { key: string | null; value: string | null } },
  key: string
) => {
  const { value } = state.auth;
  console.log('value in reducer', value);

  if (state.auth.key !== null && state.auth.key === key) {
    return value;
  }
  return null;
};

export default authSlice.reducer;
