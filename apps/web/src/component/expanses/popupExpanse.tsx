import React, { useState, useRef, useCallback } from 'react';
import { read, utils, writeFile } from 'xlsx';
import Button from '../ui/Button';
import Input from '../ui/Input';
import Styles from '../../styles/popupexpanses.module.scss';
import trashIcon from './icons/trashIcon.svg';
import exportIcon from './icons/exportIcon.svg';
import { useBulkuploadSiteExpanse } from '../../hooks/siteExpanse-hooks';
import { el } from 'date-fns/locale';
interface FileUploaderProps {
  handleFile: (file: File) => void;
  removeData: () => void;
}

interface ModalPopupProps {
  setOpenSnack(arg0: boolean): unknown;
  setMessage(arg0: string): unknown;
  reload: any;
  setReload(reload: any): unknown;
  projectId: any;
  siteId: any;
  userId: any;
  setExpanseList(data: any[]): unknown;
  closeModal: () => void;
}

const FileUploader: React.FC<FileUploaderProps> = ({
  handleFile,
  removeData,
}) => {
  const hiddenFileInput = useRef<HTMLInputElement | null>(null);
  const [fileName, setFileName] = useState<string>('');
  const handleClick = () => {
    hiddenFileInput.current?.click();
  };

  const handleFileChange = async (
    event: React.ChangeEvent<HTMLInputElement>
  ) => {
    const fileUploaded = event.target.files?.[0];
    setFileName(fileUploaded?.name || '');
    if (fileUploaded) {
      await handleFile(fileUploaded);
    }
  };

  const handleRemoveFile = () => {
    setFileName('');
    removeData();

    if (hiddenFileInput?.current) {
      const newFileInput = document.createElement('input');
      newFileInput.type = 'file';
      newFileInput.value = null;
      newFileInput.accept =
        '.csv, application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, application/vnd.ms-excel';
      newFileInput.style.display = 'none';
      newFileInput.addEventListener('change', handleFileChange);
      hiddenFileInput.current = newFileInput;
    }
  };

  return (
    <div className={Styles.inputFileWrapper}>
      <Button
        shape="rectangle"
        color="outlined"
        style={{ maxWidth: '180px' }}
        fullWidth
        justify="left"
        size="small"
        onClick={handleClick}
        type="button"
      >
        Select file
      </Button>
      <div className={Styles.fileNameWrapper}>
        <b>{fileName}</b>
        {fileName ? (
          <button
            className={Styles.removeFileButton}
            onClick={() => handleRemoveFile()}
          >
            x
          </button>
        ) : null}
      </div>
      <input
        className={Styles.fileInput}
        type="file"
        ref={hiddenFileInput}
        onChange={handleFileChange}
        accept=".csv, application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, application/vnd.ms-excel"
      />
    </div>
  );
};

const ModalPopup = (props: ModalPopupProps) => {
  const [headings, setHeadings] = useState<string[]>([]);
  const [data, setData] = useState<any[]>([]);
  const [loading, setLoading] = useState<boolean>(false);

  const removeData = () => {
    setData([]);
    setHeadings([]);
  };

  const handleImport = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    setLoading(true);
    const files = e.target.files;
    if (files && files.length) {
      const file = files[0];
      const reader = new FileReader();
      reader.onload = (event) => {
        setLoading(true);
        const wb = read(event.target?.result as ArrayBuffer);
        const sheets = wb.SheetNames;
        if (sheets.length) {
          const rows = utils.sheet_to_json(wb.Sheets[sheets[0]]);
          setData(rows);
          setHeadings(Object.keys(rows[0]));
        }
        setLoading(false);
      };
      reader.readAsArrayBuffer(file);
    }
    setLoading(false);
  }, []);

  const tableRef = useRef<HTMLTableElement>(null);

  const handleFile = async (file: File) => {
    setLoading(true);
    const reader = new FileReader();
    reader.onload = async (event) => {
      setLoading(true);
      const wb = read(event.target?.result as ArrayBuffer);
      const sheets = wb.SheetNames;
      if (sheets.length) {
        const headerSheet = utils.sheet_to_json(wb.Sheets[sheets[0]], {
          header: 1,
        });
        const header: string[] = headerSheet.shift();
        const rows = utils.sheet_to_json(wb.Sheets[sheets[0]]);
        if (rows.length)
          setHeadings([
            'description',
            'air_transport',
            'fuel',
            'labour_advance',
            'phone_stationary',
            'food_snacks',
            'purchase_service',
            'others',
            'total',
          ]);
        else setHeadings([]);
        if (header.length !== 9) alert('Please upload the correct template');
        else {
          let newData: any[] = [...rows];
          newData.forEach((ele: any) => {
            ele['total'] =
              ele['air_transport'] +
              ele['fuel'] +
              ele['labour_advance'] +
              ele['phone_stationary'] +
              ele['food_snacks'] +
              ele['purchase_service'] +
              ele['others'];
          });
          setData(newData);
        }
      }
      setLoading(false);
    };
    reader.readAsArrayBuffer(file);
    setLoading(false);
  };

  const handleValueChange = useCallback(
    async (
      e: React.ChangeEvent<HTMLInputElement>,
      index: number,
      key: string
    ) => {
      if (typeof data[index][key] === 'number') {
        setData((prevData) => {
          const newData = [...prevData];
          if (e.target.value === '') newData[index][key] = 0;
          else newData[index][key] = parseInt(e.target.value);
          newData[index]['total'] =
            newData[index]['air_transport'] +
            newData[index]['fuel'] +
            newData[index]['labour_advance'] +
            newData[index]['phone_stationary'] +
            newData[index]['food_snacks'] +
            newData[index]['purchase_service'] +
            newData[index]['others'];
          return newData;
        });
      } else {
        setData((prevData) => {
          const newData = [...prevData];
          newData[index][key] = e.target.value;
          return newData;
        });
      }
    },
    [data]
  );

  const { mutate: postbulkData } = useBulkuploadSiteExpanse();

  const handleRowRemove = async (index: number) => {
    setData((data) => {
      let newData = [...data];
      newData.splice(index, 1);
      return newData;
    });
  };

  const handleSave = (e: any) => {
    let object = {
      site_id: props.siteId,
      project_id: props.projectId,
      created_by: props.userId,
      site_expense_details: data,
    };
    postbulkData(object, {
      onSuccess(data, variables, context) {
        props.setReload(true);
        props.closeModal();
        props?.setMessage('Site Expense has been uploaded successfully !');
        props.setOpenSnack(true);
      },
    });
  };

  return (
    <div className={Styles.modal}>
      <div>
        <div className={Styles.modalHead}>
          <div>
            <b>Bulk Upload</b>
          </div>
          <div>
            <button
              className={Styles.closeModalButton}
              onClick={props.closeModal}
              shape="rounded"
              size="small"
            >
              X
            </button>
          </div>
        </div>
      </div>
      <div>
        <div>
          <div className={Styles.row}>
            <div className={Styles.col}>
              <FileUploader handleFile={handleFile} removeData={removeData} />
            </div>

            <div className={Styles.col2}>
              <Button
                shape="rectangle"
                color="primary"
                size="small"
                fullWidth
                className={Styles.maxWidth200}
                disabled={loading}
                type="button"
              >
                <div
                  style={{
                    display: 'flex',
                    alignItems: 'center',
                    justifyContent: 'space-between',
                    width: '100%',
                  }}
                >
                  <span>Export</span>
                  <img src={exportIcon} alt=""></img>
                </div>
              </Button>
            </div>
          </div>
        </div>
      </div>
      <div className="table-container">
        <div className={Styles.tableContainer}>
          {loading ? (
            <div>Loading...</div>
          ) : data.length ? (
            <table className={Styles.scrollableTable} ref={tableRef}>
              <thead>
                <tr>
                  <th>S.no</th>
                  {headings.map((ele, ind) => (
                    <th scope="col" key={ele}>
                      {ele}
                    </th>
                  ))}
                  <th></th>
                </tr>
              </thead>
              <tbody>
                {data.map((elem, index) => (
                  <tr key={index}>
                    <td>{index + 1}</td>
                    {headings.map((key) =>
                      key === 'description' ? (
                        <td key={key}>
                          <div
                            style={{
                              paddingBottom: '20px',
                              fontSize: '15px',
                              // fontWeight: 'bold',
                            }}
                          >
                            <span>{elem[key]}</span>
                          </div>
                        </td>
                      ) : key === 'total' ? (
                        <td key={key}>
                          <div
                            style={{
                              paddingBottom: '20px',
                              fontSize: '15px',
                              fontWeight: 'bold',
                            }}
                          >
                            <span>{elem[key]}</span>
                          </div>
                        </td>
                      ) : (
                        <td key={key}>
                          <Input
                            type="text"
                            onChange={(e) => handleValueChange(e, index, key)}
                            value={elem[key]}
                          />
                        </td>
                      )
                    )}
                    <td>
                      <Button
                        size="small"
                        shape="rectangle"
                        color="transparent"
                        onClick={() => handleRowRemove(index)}
                        type="button"
                      >
                        <img src={trashIcon} alt=""></img>
                      </Button>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          ) : null}
        </div>
        {data.length ? (
          <div
            className={Styles.buttonContainer}
            style={{ width: `calc(${tableRef.current?.style.width})` }}
          >
            <Button
              color="primary"
              justify="center"
              size="small"
              shape="rectangle"
              style={{ marginRight: '5px', width: '150px' }}
              type="button"
              onClick={() => {
                props.closeModal();
              }}
            >
              Cancel
            </Button>
            <Button
              color="primary"
              justify="center"
              size="small"
              shape="rectangle"
              style={{ marginRight: '5%', width: '150px' }}
              onClick={(e) => handleSave(e)}
              type="button"
            >
              Save
            </Button>
          </div>
        ) : null}
      </div>
      <div></div>
    </div>
  );
};

const PopupExpanse: React.FC = (props: any) => {
  const [modalOpen, setModalOpen] = useState<boolean>(false);
  const handleDownloadTemplate = () => {
    const headingsList = [
      [
        'description',
        'air_transport',
        'fuel',
        'labour_advance',
        'phone_stationary',
        'food_snacks',
        'purchase_service',
        'others',
        'total',
      ],
    ];
    const wb = utils.book_new();
    const ws = utils.json_to_sheet([]);
    utils.sheet_add_aoa(ws, headingsList);
    utils.book_append_sheet(wb, ws, 'Template');
    writeFile(wb, 'Template.xlsx');
  };
  return (
    <div className={Styles.container}>
      <div className={Styles.button}>
        <Button
          color="primary"
          shape="rectangle"
          justify="center"
          size="small"
          onClick={() => handleDownloadTemplate()}
          type="button"
        >
          Download
        </Button>
        <Button
          onClick={() => setModalOpen(true)}
          color="primary"
          shape="rectangle"
          justify="center"
          size="small"
          type="button"
        >
          Bulk Upload
        </Button>
      </div>

      {modalOpen && (
        <div className={Styles.modalContainer}>
          <ModalPopup
            closeModal={() => setModalOpen(false)}
            setExpanseList={props.setExpanseList}
            projectId={props.projectId}
            siteId={props.siteId}
            userId={props?.userId}
            setReload={props.setReload}
            reload={props.reload}
            openSnack={props?.openSnack}
            setOpenSnack={props?.setOpenSnack}
            message={props?.message}
            setMessage={props?.setMessage}
          />
        </div>
      )}
    </div>
  );
};

export default PopupExpanse;
