import { environment } from '../environment/environment';

export const formatBudgetValue = (value: any) => {
  const locale = environment.LOCALE;
  const currency = environment.CURRENCY;

  return value.toLocaleString(locale, {
    style: 'currency',
    currency: currency,
  });
};

// export const nullValue = (value:any) => {

// }
