import React, { useState, useRef, useCallback } from 'react';
import { read, utils, writeFile } from 'xlsx';
import Button from '../ui/Button';
import Input from '../ui/Input';
import Styles from '../../styles/popupexpanses.module.scss';
import trashIcon from './icons/trashIcon.svg';
import exportIcon from './icons/exportIcon.svg';
import { useBulkuploadSiteExpanse } from '../../hooks/siteExpanse-hooks';
interface FileUploaderProps {
  handleFile: (file: File) => void;
  removeData: () => void;
}

interface ModalPopupProps {
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
    console.log(event);
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
          console.log('rows--->', rows);

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
        console.log(rows);
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
        else setData(rows);
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
    // props.setExpanseList(data);
    let object = {
      site_id: props.siteId,
      project_id: props.projectId,
      created_by: props.userId,
      site_expense_details: data,
    };
    console.log('object', object);
    props.closeModal();
    // postbulkData(object, {
    //   onSuccess(data, variables, context) {
    //     console.log('bulkdata', data);
    //     props.setReload(true);
    //     props.closeModal();
    //   },
    // });
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
            <table className={Styles.table} ref={tableRef}>
              <thead className={Styles.tableHeading}>
                <tr className={Styles.tableRow}>
                  <th className={Styles.tableHead}>S.no</th>
                  {headings.map((ele, ind) => (
                    <th className={Styles.tableHead} scope="col" key={ele}>
                      {ele}
                    </th>
                  ))}
                  <th className={Styles.tableHead}></th>
                </tr>
              </thead>
              <tbody>
                {data.map((elem, index) => (
                  <tr className={Styles.tableRow} key={index}>
                    <th className={Styles.tableHead}>{index + 1}</th>
                    {headings.map((key) => (
                      <td className={Styles.tableData} key={key}>
                        <Input
                          transparent={true}
                          type="text"
                          onChange={(e) => handleValueChange(e, index, key)}
                          value={elem[key]}
                          errorFree={true}
                        />
                      </td>
                    ))}
                    <td className={Styles.tableData}>
                      <Button
                        size="small"
                        shape="rectangle"
                        color="transparent"
                        onClick={() => handleRowRemove(index)}
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
          />
        </div>
      )}
    </div>
  );
};

export default PopupExpanse;
