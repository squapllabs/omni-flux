import React from 'react';

interface Column {
  field: string;
  label: string;
  render?: (value: any, row: any) => JSX.Element;
}

interface DataTableProps {
  columns: Column[];
  data: any[];
  onCellClick: (value: any, field: string, rowIndex: number) => void;
}

const DataTable: React.FC<DataTableProps> = ({
  columns,
  data,
  onCellClick,
}) => {
  return (
    <table>
      <thead>
        <tr>
          {columns.map((col, index) => (
            <th key={index}>{col.label}</th>
          ))}
        </tr>
      </thead>
      <tbody>
        {data.map((row, rowIndex) => (
          <tr key={rowIndex}>
            {columns.map((col, colIndex) => (
              <td
                key={colIndex}
                onClick={() => onCellClick(row[col.field], col.field, rowIndex)}
              >
                {col.render ? col.render(row[col.field], row) : row[col.field]}
              </td>
            ))}
          </tr>
        ))}
      </tbody>
    </table>
  );
};

export default DataTable;
