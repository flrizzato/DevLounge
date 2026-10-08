object DSEmployeeServer: TDSEmployeeServer
  OldCreateOrder = False
  Height = 307
  Width = 512
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'Database=/opt/interbase/examples/employee.gdb'
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=127.0.0.1'
      'Port=3050'
      'DriverID=IB')
    LoginPrompt = False
    BeforeConnect = EmployeeConnectionBeforeConnect
    Left = 56
    Top = 16
  end
  object EmployeeTable: TFDQuery
    CachedUpdates = True
    IndexFieldNames = 'EMP_NO'
    Connection = EmployeeConnection
    UpdateObject = updEmployee
    SQL.Strings = (
      'SELECT *'
      'FROM EMPLOYEE EMP'
      'ORDER BY EMP.EMP_NO  ')
    Left = 56
    Top = 72
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 280
    Top = 16
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 168
    Top = 16
  end
  object updEmployee: TFDUpdateSQL
    Connection = EmployeeConnection
    InsertSQL.Strings = (
      'INSERT INTO EMPLOYEE'
      '(FIRST_NAME, LAST_NAME, PHONE_EXT, HIRE_DATE, '
      '  DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, '
      '  SALARY)'
      
        'VALUES (:NEW_FIRST_NAME, :NEW_LAST_NAME, :NEW_PHONE_EXT, :NEW_HI' +
        'RE_DATE, '
      
        '  :NEW_DEPT_NO, :NEW_JOB_CODE, :NEW_JOB_GRADE, :NEW_JOB_COUNTRY,' +
        ' '
      '  :NEW_SALARY)')
    ModifySQL.Strings = (
      'UPDATE EMPLOYEE'
      
        'SET FIRST_NAME = :NEW_FIRST_NAME, LAST_NAME = :NEW_LAST_NAME, PH' +
        'ONE_EXT = :NEW_PHONE_EXT, '
      
        '  HIRE_DATE = :NEW_HIRE_DATE, DEPT_NO = :NEW_DEPT_NO, JOB_CODE =' +
        ' :NEW_JOB_CODE, '
      '  JOB_GRADE = :NEW_JOB_GRADE, JOB_COUNTRY = :NEW_JOB_COUNTRY, '
      '  SALARY = :NEW_SALARY'
      'WHERE EMP_NO = :OLD_EMP_NO')
    DeleteSQL.Strings = (
      'DELETE FROM EMPLOYEE'
      'WHERE EMP_NO = :OLD_EMP_NO')
    FetchRowSQL.Strings = (
      
        'SELECT EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, HIRE_DATE, DEPT' +
        '_NO, '
      '  JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY, FULL_NAME'
      'FROM EMPLOYEE'
      'WHERE EMP_NO = :EMP_NO')
    Left = 56
    Top = 128
  end
end
