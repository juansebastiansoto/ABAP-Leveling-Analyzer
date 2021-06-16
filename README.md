# ABAP-Leveling-Analyzer
This system gets the properties and the source code of diferents objects in Development, Quality and Production enviroment and check if there is any diference.
The main goal is to check if the object has the same version in all landscape.
This is useful before starting a development and also when finishing a project to check if the project team have abandoned objects with modifications.
The system only allow objects with Z or Y development class. Local objects are ignored.

## Requirements

[ABAP HTML Mail Class](https://github.com/juansebastiansoto/ABAP-HTML-Mail-Class) to send mail with report attached.

[ABAP2XLSX](https://github.com/sapmentors/abap2xlsx) to make the attachment.

[ALV Magic Catalog](https://github.com/juansebastiansoto/ABAP-ALV-Magic-Catalog) to make the ALV field catalog.

[ABAP Utilities File Class](https://github.com/juansebastiansoto/ABAP-Utilities-File-Class) to combine all object properties into a string.

## Installation:
Install the project with [ABAP Git](https://docs.abapgit.org/)

## Configuration:

### 1.- RFC Destinations
Create two SM59 RFC destinations. One to QA Server and another to PRD Server.
Set those RFC destinations in STVARV like parameter with this names:
`PRD_RFC` for Production
`QA_RFC` for Quality

_RFC without credentials don't allow run in background. Only in foreground._

### 2.- Ignored Transport Requests

If you need ignore some objects, you can set the TR in STVARV like Select-Option with `LEVELING_TR_OUT` name.

## Scope

* ACGR - Role (beta)
* CLAS - Class (ABAP Objects)
* CUAD - GUI Definition
* DOMA - Domain
* DTEL - Data Element
* DYNP - Screen
* ENHO - Enhancement Implementation (beta)
* ENHS - Enhancement Spot (beta)
* ENQU - Lock Object
* FUGR - Function Group
* FUNC - Function Module
* METH - Method (ABAP Objects)
* PROG - Program
* REPS - Report Source Code
* REPT - Report Texts
* SHLP - Search Help
* SUSO - Authorization object
* TABD - Table Definition
* TRAN - Transaction
* TTYP - Table Type
* VIEW - View
* XSLT - Transformation

## How to use it

Run `YBCPRR0002` program with SE38 Transaction.

### 1.- Selection Screen

Fill the selection screen with your target.
You must input at least one Package, TR (or Task) or Object Type and Object Name.

### 2.- Out of scope checkbox

This checkbox will remove all objects that the system can't analyze.
Example: CDS View

### 3.- Hide OK

This checkbox will remove all objects with leveling OK at all landscape.

### 4.- E-mail Sender and Recipient

E-mail address will be set on selection-screen. If still empty, background execution is aborted.

## Output

You will have an ALV Grid with theese fields:
* Object Type
* Object Name
* QA Object Status
* PRD Object Status
* Last transport request
* TR Short Description
* Last Changed On
* Last Changed by
* Message

### Status options

![OK Icon](/assets/images/ok.png) -> OK

![Warning Icon](/assets/images/warning.png) -> Exist with differences

![Error Icon](/assets/images/error.png) -> Not exist in this server

### Example

Analyze `ZFIPRI9030` program.

![Selection-Screen Example](/assets/images/example_00.png)

![ALV Grid Output Example](/assets/images/example_01.png)

![Excel Output Example](/assets/images/example_02.png)

## Enhancement project and troubleshooting

### Add new object type

To add a new object type, you must:

1. Create a new class that inherits from `YCL_BC_LEVELING`.
2. Implement all abstract methods:
    1. `EXISTENCE_CHECK`: Verify if the object exist in the system.
    2. `GET_OBJECT_CONTENT`: Get all the content to compare (properties, source code, etc.).
    3. `DETERMINE_TEXT_OBJECT_CODE`: EUOBJALL-ID Field to get object type short text.
    4. `GET_OBJECT_LIST`: Set all objects sub objects types that the class will analyze.

### Analyze troubleshooting

If you have issues, you can active the break-point id `YBC_LEVELING_ANALYZER` with SAAB transaction.
In the SAVE_LOG method is set a LOG-POINT ID to store SERVER, HASH and DATA and raise a BREAK-POINT to open the debugger.