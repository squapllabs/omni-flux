import { environment } from '../environment/environment';

export const formatBudgetValue = (value: any) => {
  const locale = environment.LOCALE;
  const currency = environment.CURRENCY;

  return value?.toLocaleString(locale, {
    style: 'currency',
    currency: currency,
  });
};

export function customSort(order: any, sortBy: any, sortOrder: any) {
  return function (a: any, b: any) {
    const valueA = a[sortBy];
    const valueB = b[sortBy];

    if (order.includes(valueA) && order.includes(valueB)) {
      const indexA = order.indexOf(valueA);
      const indexB = order.indexOf(valueB);
      const result = indexA - indexB;
      return sortOrder === 'asc' ? result : -result;
    }

    if (valueA < valueB) {
      return sortOrder === 'asc' ? -1 : 1;
    } else if (valueA > valueB) {
      return sortOrder === 'asc' ? 1 : -1;
    } else {
      return 0;
    }
  };
}

export const handleSortByColumn = (
  columnName: any,
  sortOrder: any,
  setSortOrder: any,
  setSortColumn: any
) => {
  setSortColumn(columnName);
  setSortOrder(sortOrder === 'asc' ? 'desc' : 'asc');
};
