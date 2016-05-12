
//
// This program demonstrates how to convert a RECORD to and from an XML data string
// Each RECORD and SUB-RECORD is mapped to an XFile
//
*-----------------------------------------------------------------------------------------------------------------------------------
*-                                             Serialize RecordData into XmlData
*-----------------------------------------------------------------------------------------------------------------------------------

//
// Serialize Function
// Returns Xml string on return
//
Serialize                    FUNCTION
RecordData                    Dim               ^
                             ENTRY

Employee                     RECORD
Id                            Dim               10
FirstName                     Dim               20
LastName                      Dim               30
Ssn                           Form              9
DateOfBirth                   Form              8
Employment                    RECORD            (10)
Division                       Dim              10
Branch                         Dim              20
Department                     Dim              30
                              RECORDEND
Address                       RECORD
Street                         Dim              30(2)
City                           Dim              20
State                          Dim              2
                              RECORDEND
Salary                        RECORD            (20)
Date                           Form             8
Comment                        Dim              50(5)
Amount                         Form             6.2
                              RECORDEND
                             RECORDEND
XmlData                      Dim                4096

// Array Counters
Employee_Employment_Cntr     Form               2
Employee_Address_Street_Cntr Form               1
Employee_Salary_Cntr         Form               2
Employee_Salary_Comment_Cntr Form               1

// One XFile for each RECORD and sub-RECORD
Employee_                    XFile
Employee_Employment          XFile
Employee_Address             XFile
Employee_Salary              XFile

// Constants
XFILE_CLOSEDSCHEMA           Integer            4,"0x02"
Seq                          Form               "-1"

         // Parse the incoming record
         UnPack              RecordData,Employee

         // Create and fill the XFile with the structure data using a static schema
         //Prepare             Employee_,"c:\work\drew.xml","Employee",MODE=XFILE_CLOSEDSCHEMA,SCHEMA="Serialize_DeSerialize.xsd"
         Prepare             Employee_,"","Employee",MODE=XFILE_CLOSEDSCHEMA,SCHEMA="Serialize_DeSerialize.xsd"

         Write               Employee_,Seq;*LL,Id=Employee.Id:
                                              FirstName=Employee.FirstName:
                                              LastName=Employee.LastName:
                                              Ssn=Employee.Ssn;

         Write               Employee_,Seq;*LL,Employment=Employee_Employment;
         For                 Employee_Employment_Cntr,1,10
          Write              Employee_Employment,Seq;*LL,Division=Employee.Employment(Employee_Employment_Cntr).Division:
                                                         Branch=Employee.Employment(Employee_Employment_Cntr).Branch:
                                                         Department=Employee.Employment(Employee_Employment_Cntr).Department;
         Repeat

         Write               Employee_,Seq;*LL,Address=Employee_Address;
         For                 Employee_Address_Street_Cntr,1,2
          Write              Employee_Address,Seq;*LL,Street=Employee.Address.Street(Employee_Address_Street_Cntr);
         Repeat
         Write               Employee_Address,Seq;*LL,City=Employee.Address.City:
                                                      State=Employee.Address.State;

         Write               Employee_,Seq;*LL,Salary=Employee_Salary;
         For                 Employee_Salary_Cntr,1,20
          Write              Employee_Salary,Seq;*LL,Date=Employee.Salary(Employee_Salary_Cntr).Date;
          For                Employee_Salary_Comment_Cntr,1,5
           Write             Employee_Salary,Seq;*LL,Comment=Employee.Salary(Employee_Salary_Cntr).Comment(Employee_Salary_Comment_Cntr);
          Repeat
          Write              Employee_Salary,Seq;*LL,Amount=Employee.Salary(Employee_Salary_Cntr).Amount;
         Repeat

         // Extract the Xml from the document
         GetFile             Employee_,XMLData=XmlData
         //Close               Employee_,NOSAVE
         Close               Employee_
         Return              Using XmlData

         FUNCTIONEND

*-----------------------------------------------------------------------------------------------------------------------------------
*-                                             DeSerialize XmlData into RecordData
*-----------------------------------------------------------------------------------------------------------------------------------

//
// DeSerialize Function
// Returns a RECORD packed string
//
DeSerialize                  FUNCTION
XmlData                       Dim               ^
                             ENTRY

Employee                     RECORD
Id                            Dim               10
FirstName                     Dim               20
LastName                      Dim               30
Ssn                           Form              9
DateOfBirth                   Form              8
Employment                    RECORD            (10)
Division                       Dim              10
Branch                         Dim              20
Department                     Dim              30
                              RECORDEND
Address                       RECORD
Street                         Dim              30(2)
City                           Dim              20
State                          Dim              2
                              RECORDEND
Salary                        RECORD            (20)
Date                           Form             8
Comment                        Dim              50(5)
Amount                         Form             6.2
                              RECORDEND
                             RECORDEND
RecordData                   Dim                4096

// Array Counters
Employee_Employment_Cntr     Form               2
Employee_Address_Street_Cntr Form               1
Employee_Salary_Cntr         Form               2
Employee_Salary_Comment_Cntr Form               1

// One XFile for each RECORD and sub-RECORD
Employee_                    XFile
Employee_Employment          XFile
Employee_Address             XFile
Employee_Salary              XFile

// Constants
XFILE_CLOSEDSCHEMA           Integer            4,"0x02"
Seq                          Form               "-1"

         // Prepare
         UnPack              "",Employee

         // Create and fill the XFile with the structure data using a static schema
         Open                Employee_,"Employee",Data=XmlData,MODE=XFILE_CLOSEDSCHEMA,SCHEMA="Serialize_DeSerialize.xsd"

         Read                Employee_,Seq;*LL,Id=Employee.Id:
                                              FirstName=Employee.FirstName:
                                              LastName=Employee.LastName:
                                              Ssn=Employee.Ssn;

         Read                Employee_,Seq;*LL,Employment=Employee_Employment;
         For                 Employee_Employment_Cntr,1,10
          Read               Employee_Employment,Seq;*LL,Division=Employee.Employment(Employee_Employment_Cntr).Division:
                                                         Branch=Employee.Employment(Employee_Employment_Cntr).Branch:
                                                         Department=Employee.Employment(Employee_Employment_Cntr).Department;
         Repeat              If Not Over

         Read                Employee_,Seq;*LL,Address=Employee_Address;
         For                 Employee_Address_Street_Cntr,1,2
          Read               Employee_Address,Seq;*LL,Street=Employee.Address.Street(Employee_Address_Street_Cntr);
         Repeat              If Not Over
         Read                Employee_Address,Seq;*LL,City=Employee.Address.City:
                                                      State=Employee.Address.State;

         Read                Employee_,Seq;*LL,Salary=Employee_Salary;
         For                 Employee_Salary_Cntr,1,20
          Read               Employee_Salary,Seq;*LL,Date=Employee.Salary(Employee_Salary_Cntr).Date;
          For                Employee_Salary_Comment_Cntr,1,5
           Read              Employee_Salary,Seq;*LL,Comment=Employee.Salary(Employee_Salary_Cntr).Comment(Employee_Salary_Comment_Cntr);
          Repeat             If Not Over
          Read               Employee_Salary,Seq;*LL,Amount=Employee.Salary(Employee_Salary_Cntr).Amount;
         Repeat              If Not Over
         Close               Employee_,NOSAVE

         // Construct the RECORD string data
         PackKEY             RecordData,Employee
         Return              Using RecordData

         FUNCTIONEND
