!> Default RTL error handler. Changes RTL errors into exceptions.
!! Do not call this function.
!! @param integer, intent(in) :: AError
!!    The error code number from IOSTAT
!! @param logical, intent(in) :: AContinuable
!!    Specifies whether a condition is continuable
!! @param character(*), intent(in) :: AMessage
!!    The error message as contructed by the RTL
!! @param integer(INT_PTR_KIND()), intent(in) :: AContext
!!    The context specified for establishQQ
!! @return logical
!!    .true. means that the error has been handled and the application should not
!!    issue an error message. .false. means that the application should issue an
!!    error message and continue as if there had never been a handler.
!!    This handler by default issues .true.
!! @throws Exception
!!    and all kinds of subtypes, depending on the error code
logical function Exception$establishHandler(AError, AContinuable, AMessage, AContext) &
   result(Res)
   integer, intent(in) :: AError
   logical, intent(in) :: AContinuable
   character(*), intent(in) :: AMessage
   integer(INT_PTR_KIND()), intent(in) :: AContext

   Res = .true.
   select case(AError)
      case(FOR$IOS_EOR)
         ! I/O (-2): End-of-record condition with non-advancing records
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_EOF)
         ! I/O (-1): Read beyond end of file
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_NOTFORSPE)
         ! An error in the user program or in the RTL was not an Intel® Fortran-specific
         ! error and was not reportable through any other Intel® Fortran run-time
         ! messages.
         ! severe (1): Not a Fortran-specific error
         throw(Exception(AMessage, AError))
      case(FOR$IOS_BUG_CHECK)
         ! Internal error. Please check that the program is correct. Recompile if an
         ! error existed in the program. If this error persists, submit a problem report.
         ! severe (8): Internal consistency check failure
         throw(EAssertionFailed(AMessage, AError))
      case(FOR$IOS_PERACCFIL)
         ! Check the permissions of the specified file and whether the network device is
         ! mapped and available. Make sure the correct file and device was being
         ! accessed. Change the protection, specific file, or process used before
         ! rerunning the program.
         ! severe (9): Permission to access file denied
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_CANOVEEXI)
         ! Specified file xxx already exists when OPEN statement specified STATUS='NEW'
         ! (create new file) using I/O unit x. Make sure correct file name, directory
         ! path, unit, and so forth were specified in the source program. Decide whether
         ! to:
         ! - Rename or remove the existing file before rerunning the program.
         ! - Modify the source file to specify different file specification, I/O unit, or
         !   OPEN statement STATUS.
         ! severe (10): Cannot overwrite existing file
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_UNINOTCON)
         ! The specified unit was not open at the time of the attempted I/O operation.
         ! Check if correct unit number was specified. If appropriate, use an OPEN
         ! statement to explicitly open the file (connect the file to the unit number).
         ! info (11): Unit not connected
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_SYNERRNAM)
         ! The syntax of input to a namelist-directed READ statement was incorrect.
         ! severe (17): Syntax error in NAMELIST input
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_TOOMANVAL)
         ! An attempt was made to assign too many values to a variable during a namelist
         ! READ statement.
         ! severe (18): Too many values for NAMELIST variable
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_INVREFVAR)
         ! One of the following conditions occurred:
         ! - The variable was not a member of the namelist group.
         ! - An attempt was made to subscript a scalar variable.
         ! - A subscript of the array variable was out-of-bounds.
         ! - An array variable was specified with too many or too few subscripts for the
         !   variable.
         ! - An attempt was made to specify a substring of a non-character variable or
         !   array name.
         ! - A substring specifier of the character variable was out-of-bounds.
         ! - A subscript or substring specifier of the variable was not an integer
         !   constant.
         ! - An attempt was made to specify a substring by using an unsubscripted array
         !   variable.
         ! severe (19): Invalid reference to variable in NAMELIST input
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_REWERR)
         ! One of the following conditions occurred:
         ! - The file was not a sequential file.
         ! - The file was not opened for sequential or append access.
         ! - The Intel® Fortran RTL I/O system detected an error condition during
         !   execution of a REWIND statement.
         ! severe (20): REWIND error
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_DUPFILSPE)
         ! Multiple attempts were made to specify file attributes without an intervening
         ! close operation. A DEFINE FILE statement was followed by another DEFINE FILE
         ! statement or an OPEN statement.
         ! severe (21): Duplicate file specifications
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_INPRECTOO)
         ! A record was read that exceeded the explicit or default record length
         ! specified when the file was opened. To read the file, use an OPEN statement
         ! with a RECL= value (record length) of the appropriate size.
         ! severe (22): Input record too long
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_BACERR)
         ! The Intel® Fortran RTL I/O system detected an error condition during execution
         ! of a BACKSPACE statement.
         ! severe (23): BACKSPACE error
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_ENDDURREA)
         ! One of the following conditions occurred:
         ! - An Intel® Fortran RTL I/O system end-of-file condition was encountered
         !   during execution of a READ statement that did not contain an END, ERR, or
         !   IOSTAT specification.
         ! - An end-of-file record written by the ENDFILE statement was encountered
         !   during execution of a READ statement that did not contain an END, ERR, or
         !   IOSTAT specification.
         ! - An attempt was made to read past the end of an internal file character
         !   string or array during execution of a READ statement that did not contain an
         !   END, ERR, or IOSTAT specification.
         ! This error is returned by END and ERRSNS.
         ! severe (24): End-of-file during read
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_RECNUMOUT)
         ! A direct access READ, WRITE, or FIND statement specified a record number
         ! outside the range specified when the file was opened.
         ! severe (25): Record number outside range
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_OPEDEFREQ)
         ! A direct access READ, WRITE, or FIND statement was attempted for a file when
         ! no prior DEFINE FILE or OPEN statement with ACCESS='DIRECT' was performed for
         ! that file.
         ! severe (26): OPEN or DEFINE FILE required
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_TOOMANREC)
         ! An attempt was made to do one of the following:
         ! - Read or write more than one record with an ENCODE or DECODE statement.
         ! - Write more records than existed.
         ! severe (27): Too many records in I/O statement
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_CLOERR)
         ! An error condition was detected by the Intel® Fortran RTL I/O system during
         ! execution of a CLOSE statement.
         ! severe (28): CLOSE error
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_FILNOTFOU)
         ! A file with the specified name could not be found during an OPEN operation.
         ! severe (29): File not found
         throw(EFileNotFoundException(AMessage, AError))
      case(FOR$IOS_OPEFAI)
         ! An error was detected by the Intel® Fortran RTL I/O system while attempting to
         ! open a file in an OPEN, INQUIRE, or other I/O statement. This message is
         ! issued when the error condition is not one of the more common conditions for
         ! which specific error messages are provided. It can occur when an OPEN
         ! operation was attempted for one of the following:
         ! - Segmented file that was not on a disk or a raw magnetic tape
         ! - Standard I/O file that had been closed
         ! severe (30): Open failure
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_MIXFILACC)
         ! An attempt was made to use any of the following combinations:
         ! - Formatted and unformatted operations on the same unit
         ! - An invalid combination of access modes on a unit, such as direct and
         !   sequential
         ! - An Intel® Fortran RTL I/O statement on a logical unit that was opened by a
         !   program coded in another language
         ! severe (31): Mixed file access modes
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_INVLOGUNI)
         ! A logical unit number greater than 2,147,483,647 or less than zero was used in
         ! an I/O statement.
         ! severe (32): Invalid logical unit number
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_ENDFILERR)
         ! One of the following conditions occurred:
         ! - The file was not a sequential organization file with variable-length
         !   records.
         ! - The file was not opened for sequential, append, or direct access.
         ! - An unformatted file did not contain segmented records.
         ! - The Intel® Fortran RTL I/O system detected an error during execution of an
         !   ENDFILE statement.
         ! severe (33): ENDFILE error
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_UNIALROPE)
         ! A DEFINE FILE statement specified a logical unit that was already opened.
         ! severe (34): Unit already open
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_SEGRECFOR)
         ! An invalid segmented record control data word was detected in an unformatted
         ! sequential file. The file was probably either created with RECORDTYPE='FIXED'
         ! or 'VARIABLE' in effect, or was created by a program written in a language
         ! other than Fortran or Standard Fortran.
         ! severe (35): Segmented record format error
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_ATTACCNON)
         ! A direct-access READ or FIND statement attempted to access beyond the end of a
         ! relative file (or a sequential file on disk with fixed-length records) or
         ! access a record that was previously deleted from a relative file.
         ! severe (36): Attempt to access non-existent record
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_INCRECLEN)
         ! An attempt was made to open a direct access file without specifying a record
         ! length.
         ! severe (37): Inconsistent record length
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_ERRDURWRI)
         ! The Intel® Fortran RTL I/O system detected an error condition during execution
         ! of a WRITE statement.
         ! severe (38): Error during write
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_ERRDURREA)
         ! The Intel® Fortran RTL I/O system detected an error condition during execution
         ! of a READ statement.
         ! severe (39): Error during read
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_RECIO_OPE)
         ! While processing an I/O statement for a logical unit, another I/O operation on
         ! the same logical unit was attempted, such as a function subprogram that
         ! performs I/O to the same logical unit that was referenced in an expression in
         ! an I/O list or variable format expression.
         ! severe (40): Recursive I/O operation
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_INSVIRMEM)
         ! The Intel® Fortran RTL attempted to exceed its available virtual memory while
         ! dynamically allocating space. To overcome this problem, investigate increasing
         ! the data limit. Before you try to run this program again, wait until the new
         ! system resources take effect.
         ! Note:
         ! This error can be returned by STAT in an ALLOCATE or a DEALLOCATE statement.
         ! severe (41): Insufficient virtual memory
         call Exception$throwHeapException(HeapExceptions$IOS_INSVIRMEM)
      case(FOR$IOS_NO_SUCDEV)
         ! A pathname included an invalid or unknown device name when an OPEN operation
         ! was attempted.
         ! severe (42): No such device
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_FILNAMSPE)
         ! A pathname or file name given to an OPEN or INQUIRE statement was not
         ! acceptable to the Intel® Fortran RTL I/O system.
         ! severe (43): File name specification error
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_INCRECTYP)
         ! The RECORDTYPE value in an OPEN statement did not match the record type
         ! attribute of the existing file that was opened.
         ! severe (44): Inconsistent record type
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_KEYVALERR)
         ! An improper value was specified for an OPEN or CLOSE statement specifier
         ! requiring a value.
         ! severe (45): Keyword value error in OPEN statement
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_INCOPECLO)
         ! Specifications in an OPEN or CLOSE statement were inconsistent. Some invalid
         ! combinations follow:
         ! - READONLY or ACTION='READ' with STATUS='NEW' or STATUS='SCRATCH'
         ! - READONLY with STATUS='REPLACE', ACTION='WRITE', or ACTION='READWRITE'
         ! - ACCESS='APPEND' with READONLY, ACTION='READ', STATUS='NEW', or STATUS='SCRATCH'
         ! - DISPOSE='SAVE', 'PRINT', or 'SUBMIT' with STATUS='SCRATCH'
         ! - DISPOSE='DELETE' with READONLY
         ! - CLOSE statement STATUS='DELETE' with OPEN statement READONLY
         ! - ACCESS='DIRECT' with POSITION='APPEND' or 'ASIS'
         ! severe (46): Inconsistent OPEN/CLOSE parameters
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_WRIREAFIL)
         ! A write operation was attempted to a file that was declared ACTION='READ' or
         ! READONLY in the OPEN statement that is currently in effect.
         ! severe (47): Write to READONLY file
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_INVARGFOR)
         ! The compiler passed an invalid or improperly coded argument to the Intel®
         ! Fortran RTL. This can occur if the compiler is newer than the RTL in use.
         ! severe (48): Invalid argument to Fortran Run-Time Library
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_INCFILORG)
         ! The file organization specified in an OPEN statement did not match the
         ! organization of the existing file.
         ! severe (51): Inconsistent file organization
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_NO_CURREC)
         ! Attempted to execute a REWRITE statement to rewrite a record when the current
         ! record was undefined. To define the current record, execute a successful READ
         ! statement. You can optionally perform an INQUIRE statement on the logical unit
         ! after the READ statement and before the REWRITE statement. No other operations
         ! on the logical unit may be performed between the READ and REWRITE statements.
         ! severe (53): No current record
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_DELERR)
         ! An error condition was detected by the Intel® Fortran RTL I/O system during
         ! execution of a DELETE statement.
         ! severe (55): DELETE error
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_FINERR)
         ! The Intel® Fortran RTL I/O system detected an error condition during execution
         ! of a FIND statement.
         ! severe (57): FIND error
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_FMTSYN)
         ! Check the statement containing xx, a character substring from the format
         ! string, for a format syntax error. For more information, see the FORMAT
         ! statement.
         ! info (58): Format syntax error at or near xx
         throw(EFormatError(AMessage, AError))
         ! We always issue this as an error, since it is a runtime message that cannot be
         ! suppressed.
      case(FOR$IOS_LISIO_SYN)
         ! The data in a list-directed input record had an invalid format, or the type of
         ! the constant was incompatible with the corresponding variable. The value of
         ! the variable was unchanged.
         ! Note:
         ! The ERR transfer is taken after completion of the I/O statement for error
         ! number 59. The resulting file status and record position are the same as if no
         ! error had occurred. However, other I/O errors take the ERR transfer as soon as
         ! the error is detected, so file status and record position are undefined.
         ! severe (59): List-directed I/O syntax error
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_INFFORLOO)
         ! The format associated with an I/O statement that included an I/O list had no
         ! field descriptors to use in transferring those values.
         ! severe (60): Infinite format loop
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_FORVARMIS)
         ! An attempt was made either to read or write a real variable with an integer
         ! field descriptor (I, L, O, Z, B), or to read or write an integer or logical
         ! variable with a real field descriptor (D, E, or F). To suppress this error
         ! message, see the description of check[:]noformat.
         ! Note:
         ! The severity depends on the check[:]keywords option used during the
         ! compilation command. The ERR transfer is taken after completion of the I/O
         ! statement for error numbers 61, 63, 64, and 68. The resulting file status and
         ! record position are the same as if no error had occurred. However, other I/O
         ! errors take the ERR transfer as soon as the error is detected, so file status
         ! and record position are undefined.
         ! severe or info(61): Format/variable-type mismatch
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_SYNERRFOR)
         ! A syntax error was encountered while the RTL was processing a format stored in
         ! an array or character variable.
         ! severe (62): Syntax error in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_OUTCONERR)
         ! During a formatted output operation, the value of a particular number could
         ! not be output in the specified field length without loss of significant
         ! digits. When this situation is encountered, the overflowed field is filled
         ! with asterisks to indicate the error in the output record. If no ERR address
         ! has been defined for this error, the program continues after the error message
         ! is displayed. To suppress this error message, see the description of
         ! check[:]nooutput_conversion.
         ! Note:
         ! The severity depends on the check[:]keywords option used during the
         ! compilation command. The ERR transfer is taken after completion of the I/O
         ! statement for error numbers 61, 63, 64, and 68. The resulting file status and
         ! record position are the same as if no error had occurred. However, other I/O
         ! errors take the ERR transfer as soon as the error is detected, so file status
         ! and record position are undefined.
         ! error or info(63): Output conversion error
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_INPCONERR)
         ! During a formatted input operation, an invalid character was detected in an
         ! input field, or the input value overflowed the range representable in the
         ! input variable. The value of the variable was set to zero.
         ! Note:
         ! The ERR transfer is taken after completion of the I/O statement for error
         ! numbers 61, 63, 64, and 68. The resulting file status and record position are
         ! the same as if no error had occurred. However, other I/O errors take the ERR
         ! transfer as soon as the error is detected, so file status and record position
         ! are undefined.
         ! severe (64): Input conversion error
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_FLTINV)
         ! During an arithmetic operation, the floating-point values used in a
         ! calculation were invalid for the type of operation requested or invalid
         ! exceptional values. For example, the error can occur if you request a log of
         ! the floating-point values 0.0 or a negative number. For certain arithmetic
         ! expressions, specifying the check[:]nopower option can suppress this message.
         ! error (65): Floating invalid
         throw(EInvalidOp(AMessage, AError))
      case(FOR$IOS_OUTSTAOVE)
         ! An output statement attempted to transfer more data than would fit in the
         ! maximum record size.
         ! severe (66): Output statement overflows record
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_INPSTAREQ)
         ! Attempted to read more data than exists in a record with an unformatted READ
         ! statement or with a formatted sequential READ statement from a file opened
         ! with a PAD='NO' specifier.
         ! severe (67): Input statement requires too much data
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_VFEVALERR)
         ! The value of a variable format expression was not within the range acceptable
         ! for its intended use; for example, a field width was less than or equal to
         ! zero. A value of 1 was assumed, except for a P edit descriptor, for which a
         ! value of zero was assumed.
         ! Note:
         ! The ERR transfer is taken after completion of the I/O statement for error
         ! numbers 61, 63, 64, and 68. The resulting file status and record position are
         ! the same as if no error had occurred. However, other I/O errors take the ERR
         ! transfer as soon as the error is detected, so file status and record position
         ! are undefined.
         ! severe (68): Variable format expression value error
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_SIGINT)
         ! The process received the signal SIGINT. Determine source of this interrupt
         ! signal (described in signal(3)).
         ! error (69): Process interrupted (SIGINT)
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_INTOVF)
         ! During an arithmetic operation, an integer value exceeded byte, word, or
         ! longword range. The result of the operation was the correct low-order part.
         ! Consider specifying a larger integer data size (modify source program or, for
         ! an INTEGER declaration, possibly use the integer-size[:]size option).
         ! severe (70): Integer overflow
         throw(EIntOverflow(AMessage, AError))
      case(FOR$IOS_INTDIV)
         ! During an integer arithmetic operation, an attempt was made to divide by zero.
         ! The result of the operation was set to the dividend, which is equivalent to
         ! division by 1.
         ! severe (71): Integer divide by zero
         throw(EDivByZero(AMessage, AError))
      case(FOR$IOS_FLTOVF)
         ! During an arithmetic operation, a floating-point value exceeded the largest
         ! representable value for that data type. See Data Representation for ranges of
         ! the various data types.
         ! error (72): Floating overflow
         throw(EOverflow(AMessage, AError))
      case(FOR$IOS_FLTDIV)
         ! During a floating-point arithmetic operation, an attempt was made to divide by
         ! zero.
         ! error (73): Floating divide by zero
         throw(EZeroDivide(AMessage, AError))
      case(FOR$IOS_FLTUND)
         ! During an arithmetic operation, a floating-point value became less than the
         ! smallest finite value for that data type. Depending on the values of the
         ! fpe[:]n option, the underflowed result was either set to zero or allowed to
         ! gradually underflow. See the Data Representation for ranges of the various
         ! data types.
         ! error (74): Floating underflow
         throw(EUnderflow(AMessage, AError))
      case(FOR$IOS_SIGFPE)
         ! A floating-point exception occurred. Possible causes include:
         ! - Division by zero.
         ! - Overflow.
         ! - An invalid operation, such as subtraction of infinite values, multiplication
         !   of zero by infinity without signs), division of zero by zero or infinity by
         !   infinity.
         ! - Conversion of floating-point to fixed-point format when an overflow prevents
         !   conversion.
         ! error (75): Floating point exception
         throw(EMathError(AMessage, AError))
      case(FOR$IOS_SIGIOT)
         ! Core dump file created. Examine core dump for possible cause of this IOT
         ! signal.
         ! error (76): IOT trap signal
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_SUBRNG)
         ! An array reference was detected outside the declared array bounds.
         ! severe (77): Subscript out of range
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_SIGTERM)
         ! The process received a signal requesting termination of this process.
         ! Determine the source of this software termination signal.
         ! error (78): Process killed
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_SIGQUIT)
         ! The process received a signal requesting termination of itself. Determine the
         ! source of this quit signal.
         ! error (79): Process quit
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_IOSTAT_INTERNAL)
         ! The file name specified for an INQUIRE statement is an internal unit-number.
         ! info (90): INQUIRE of internal unit-number is always an error (NOTE: unit
         ! identifies a file)
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_INTERNAL_UNIT)
         ! The unit number specified for an INQUIRE statement is an internal unit-number.
         ! info (91): INQUIRE of internal unit-number is always an error (NOTE: unit does
         ! not identify a file)
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_FLOCONFAI)
         ! The attempted unformatted read or write of nonnative floating-point data
         ! failed because the floating-point value:
         ! - Exceeded the allowable maximum value for the equivalent native format and
         !   was set equal to infinity (plus or minus).
         ! - Was infinity (plus or minus) and was set to infinity (plus or minus).
         ! - Was invalid and was set to not a number (NaN).
         ! Very small numbers are set to zero (0). This error could be caused by the
         ! specified nonnative floating-point format not matching the floating-point
         ! format found in the specified file. Check the following:
         ! - The correct file was specified.
         ! - The record layout matches the format Intel® Fortran is expecting.
         ! - The ranges for the data being used (see Data Representation).
         ! - The correct nonnative floating-point data format was specified (see
         !   Supported Native and Nonnative Numeric Formats).
         ! info (95): Floating-point conversion failed
         throw(EMathError(AMessage, AError))
      case(FOR$IOS_UFMTENDIAN)
         ! Syntax for specifying whether little endian or big endian conversion is
         ! performed for a given Fortran unit was incorrect. Even though the program will
         ! run, the results might not be correct if you do not change the value of
         ! F_UFMTENDIAN. For correct syntax, see Environment Variable F_UFMTENDIAN
         ! Method.
         ! info (96): F_UFMTENDIAN environment variable was ignored:erroneous syntax
         throw(EInvalidSyntax(AMessage, AError))
      case(FOR$IOS_NOMEMFORIO)
         ! This error often occurs during a file I-O operation such as OPEN, READ, or
         ! WRITE. Either increase the amount of memory available to the program, or
         ! reduce its demand.
         ! severe (98): cannot allocate memory for the file buffer - out of memory
         call Exception$throwHeapException(HeapExceptions$IOS_NOMEMFORIO)
      case(FOR$IOS_INVBLOCKSIZE)
         ! Syntax for specifying the default block size value was incorrect. For correct
         ! syntax, see Environment Variable FORT_BLOCKSIZE.
         ! severe (106): FORT_BLOCKSIZE environment variable has erroneous syntax
         throw(EInvalidSyntax(AMessage, AError))
      case(FOR$IOS_INVBUFRCNT)
         ! Syntax for specifying the default buffer count value was incorrect. For
         ! correct syntax, see Environment Variable FORT_BUFFERCOUNT.
         ! severe (107): FORT_BUFFERCOUNT environment variable has erroneous syntax
         throw(EInvalidSyntax(AMessage, AError))
      case(FOR$IOS_CANSTAFIL)
         ! Make sure correct file and unit were specified.
         ! severe (108): Cannot stat file
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_SIONOTOPEN)
         ! Stream data transfer statement is not allowed to an unopened unit.
         ! info (109): stream data transfer statement is not allowed to an unopened unit
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_MAX_FXD_RECL)
         ! The 'RECL=' value in an OPEN statement is too large for the file's record
         ! type.
         ! severe (118): The 'RECL=' value in an OPEN statement exceeds the maximum
         ! allowed for the file's record type.
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_INVTHRESHOLD)
         ! If specified, the FORT_BUFFERING_THRESHOLD environment variable must be an
         ! integer value greater than zero and less than 2147483647.
         ! severe (119): The FORT_BUFFERING_THRESHOLD environment variable has erroneous
         ! syntax
         throw(EInvalidSyntax(AMessage, AError))
      case(FOR$IOS_OPEREQSEE)
         ! Attempted an operation on a file that requires the ability to perform seek
         ! operations on that file. Make sure the correct unit, directory path, and file
         ! were specified.
         ! severe (120): Operation requires seek ability
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_INVCMDECL)
         ! The command line passed to the EXECUTE_COMMAND_LINE intrinsic is invalid.
         ! Note:
         ! This error can be returned by CMDSTAT in an EXECUTE_COMMAND_LINE statement.
         ! severe (124): Invalid command supplied to EXECUTE_COMMAND_LINE
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_BRK_BD_NOTTAKEN)
         ! Program was terminated internally through abort().
         ! severe (134): Program was terminated internally through abort().
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_BRK_RANGE)
         ! An array subscript is outside the dimensioned boundaries of that array.
         ! Recompile with the check[:]bounds option set.
         ! severe (138): Array index out of bounds
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_BRK_RANGE2)
         ! An array subscript is outside the dimensioned boundaries of that array.
         ! Recompile with the check[:]bounds option set.
         ! severe: (139): Array index out of bounds for index nn
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_FLTINE)
         ! A floating-point arithmetic or conversion operation gave a result that differs
         ! from the mathematically exact result. This trap is reported if the rounded
         ! result of an IEEE operation is not exact.
         ! error (140): Floating inexact
         throw(EInexact(AMessage, AError))
      case(FOR$IOS_ROPRAND)
         ! The Intel® Fortran RTL encountered a reserved operand while executing your
         ! program. Please report the problem to Intel.
         ! severe (144): Reserved operand
         throw(Exception(AMessage, AError))
      case(FOR$IOS_ASSERTERR)
         ! The Intel® Fortran RTL encountered an assertion error. Please report the
         ! problem to Intel.
         ! severe (145): Assertion error
         throw(EAssertionFailed(AMessage, AError))
      case(FOR$IOS_NULPTRERR)
         ! Attempted to use a pointer that does not contain an address. Modify the source
         ! program, recompile, and relink.
         ! severe (146): Null pointer error
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_STKOVF)
         ! The Intel® Fortran RTL encountered a stack overflow while executing your
         ! program.
         ! severe (147): Stack overflow
         throw(EStackOverflow(AMessage, AError))
      case(FOR$IOS_STRLENERR)
         ! During a string operation, an integer value appears in a context where the
         ! value of the integer is outside the permissible string length range. Recompile
         ! with the check[:]bounds option.
         ! severe (148): String length error
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_SUBSTRERR)
         ! An array subscript is outside the dimensioned boundaries of an array.
         ! Recompile with the check[:]bounds option.
         ! severe (149): Substring error
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_RANGEERR)
         ! An integer value appears in a context where the value of the integer is
         ! outside the permissible range.
         ! severe (150): Range error
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_INVREALLOC)
         ! An allocatable array must not already be allocated when you attempt to
         ! allocate it. You must deallocate the array before it can again be allocated.
         ! Note:
         ! This error can be returned by STAT in an ALLOCATE statement.
         ! severe (151): Allocatable array is already allocated
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_RESACQFAI)
         ! Failed to acquire an Intel® Fortran RTL global resource for a reentrant
         ! routine. For a multithreaded program, the requested global resource is held by
         ! a different thread in your program. For a program using asynchronous handlers,
         ! the requested global resource is held by the calling part of the program
         ! (such as main program) and your asynchronous handler attempted to acquire the
         ! same global resource.
         ! severe (152): Unresolved contention for Intel Fortran RTL global resource
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_INVDEALLOC)
         ! A Standard Fortran allocatable array or pointer must already be allocated when
         ! you attempt to deallocate it. You must allocate the array or pointer before it
         ! can again be deallocated.
         ! Note:
         ! This error can be returned by STAT in an DEALLOCATE statement.
         ! severe (153): Allocatable array or pointer is not allocated
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_RANGE)
         ! An array subscript is outside the dimensioned boundaries of that array.
         ! Recompile with the check[:]bounds option set.
         ! severe(154): Array index out of bounds
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_RANGE2)
         ! An array subscript is outside the dimensioned boundaries of that array.
         ! Recompile with the check[:]bounds option set.
         ! severe(155): Array index out of bounds for index nn
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_DEF_GENTRAP)
         ! The Intel® Fortran RTL has detected an unknown GENTRAP code. The cause is most
         ! likely a software problem due to memory corruption, or software signaling an
         ! exception with an incorrect exception code. Try recompiling with the
         ! check[:]bounds option set to see if that finds the problem.
         ! severe(156): GENTRAP code = hex dec
         throw(EExternalException(AMessage, AError))
      case(FOR$IOS_ACCVIO)
         ! The program tried to read from or write to a virtual address for which it does
         ! not have the appropriate access. Try recompiling with the check[:]bounds
         ! option set, to see if the problem is an out-of-bounds memory reference or a
         ! argument mismatch that causes data to be treated as an address.
         ! Other causes of this error include:
         ! - Mismatches in C vs. STDCALL calling mechanisms, causing the stack to become corrupted.
         ! - References to unallocated pointers.
         ! - Attempting to access a protected (for example, read-only) address.
         ! severe(157): Program Exception - access violation
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_DTYPE_MISALIGN)
         ! The Intel® Fortran RTL has detected data that is not aligned on a natural
         ! boundary for the data type specified. For example, a REAL(8) data item aligned
         ! on natural boundaries has an address that is a multiple of 8. To ensure
         ! naturally aligned data, use the align option.
         ! This is an operating system error. See your operating system documentation for
         ! more information.
         ! severe(158): Program Exception - datatype misalignment
         throw(EOSError(AMessage, AError))
      case(FOR$IOS_PGM_BPT)
         ! The Intel® Fortran RTL has encountered a breakpoint in the program.
         ! This is an operating system error. See your operating system documentation for
         ! more information.
         ! severe(159): Program Exception - breakpoint
         throw(EOSError(AMessage, AError))
      case(FOR$IOS_PGM_SS)
         ! A trace trap or other single-instruction mechanism has signaled that one
         ! instruction has been executed.
         ! This is an operating system error. See your operating system documentation for
         ! more information.
         ! severe(160): Program Exception - single step
         throw(EOSError(AMessage, AError))
      case(FOR$IOS_PGM_BOUNDS)
         ! The program tried to access an array element that is outside the specified
         ! boundaries of the array. Recompile with the check[:]bounds option set.
         ! severe(161): Program Exception - array bounds exceeded
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_PGM_DENORM)
         ! A floating-point arithmetic or conversion operation has a denormalized number
         ! as an operand. A denormalized number is smaller than the lowest value in the
         ! normal range for the data type specified. See Data Representation for ranges
         ! for floating-point types.
         ! Either locate and correct the source code causing the denormalized value or,
         ! if a denormalized value is acceptable, specify a different value for the fpe
         ! compiler option to allow program continuation.
         ! severe(162): Program Exception - denormal floating-point operand
         throw(EUnderflow(AMessage, AError))
      case(FOR$IOS_PGM_FLTSTK)
         ! During a floating-point operation, the floating-point register stack on
         ! systems using IA-32 architecture overflowed or underflowed. This is a fatal
         ! exception. The most likely cause is calling a REAL function as if it were an
         ! INTEGER function or subroutine, or calling an INTEGER function or subroutine
         ! as if it were a REAL function.
         ! Carefully check that the calling code and routine being called agree as to how
         ! the routine is declared. If you are unable to resolve the issue, please send a
         ! problem report with an example to Intel.
         ! severe(163): Program Exception - floating stack check
         throw(EMathError(AMessage, AError))
      case(FOR$IOS_PGM_INTDIV)
         ! During an integer arithmetic operation, an attempt was made to divide by zero.
         ! Locate and correct the source code causing the integer divide by zero.
         ! severe(164): Program Exception - integer divide by zero
         throw(EDivByZero(AMessage, AError))
      case(FOR$IOS_PGM_INTOVF)
         ! During an arithmetic operation, an integer value exceeded the largest
         ! representable value for that data type. See Data Representation for ranges for
         ! INTEGER types.
         ! severe(165): Program Exception - integer overflow
         throw(EIntOverflow(AMessage, AError))
      case(FOR$IOS_PGM_PRIVINST)
         ! The program tried to execute an instruction whose operation is not allowed in
         ! the current machine mode.
         ! This is an operating system error. See your operating system documentation for
         ! more information.
         ! severe(166): Program Exception - privileged instruction
         throw(EPrivilege(AMessage, AError))
      case(FOR$IOS_PGM_INPGERR)
         ! The program tried to access a page that was not present, so the system was
         ! unable to load the page. For example, this error might occur if a network
         ! connection was lost while trying to run a program over the network.
         ! This is an operating system error. See your operating system documentation for
         ! more information.
         ! severe(167): Program Exception - in page error
         throw(EOSError(AMessage, AError))
      case(FOR$IOS_PGM_ILLINST)
         ! The program tried to execute an invalid instruction.
         ! This is an operating system error. See your operating system documentation for
         ! more information.
         ! severe(168): Program Exception - illegal instruction
         throw(EOSError(AMessage, AError))
      case(FOR$IOS_PGM_NOCONTEXCP)
         ! The program tried to continue execution after a noncontinuable exception
         ! occurred.
         ! This is an operating system error. See your operating system documentation for
         ! more information.
         ! severe(169): Program Exception - noncontinuable exception
         Res = .false.
         ! If the operating system does not want us to continue, who are we to ignore
         ! this?
      case(FOR$IOS_PGM_STKOVF)
         ! The Intel® Fortran RTL has detected a stack overflow while executing your
         ! program. See your Release Notes for information on how to increase stack size.
         ! severe(170): Program Exception - stack overflow
         throw(EStackOverflow(AMessage, AError))
      case(FOR$IOS_PGM_INVDISP)
         ! An exception handler returned an invalid disposition to the exception
         ! dispatcher. Programmers using a high-level language should never encounter
         ! this exception.
         ! This is an operating system error. See your operating system documentation for
         ! more information.
         ! severe(171): Program Exception - invalid disposition
         throw(EOSError(AMessage, AError))
      case(FOR$IOS_PGM_EXCP_CODE)
         ! The Intel® Fortran RTL has detected an unknown exception code.
         ! This is an operating system error. See your operating system documentation for
         ! more information.
         ! severe(172): Program Exception - exception code = hex dec
         throw(EOSError(AMessage, AError))
      case(FOR$IOS_INVDEALLOC2)
         ! A pointer that was passed to DEALLOCATE pointed to an explicit array, an array
         ! slice, or some other type of memory that could not be deallocated in a
         ! DEALLOCATE statement. Only whole arrays previous allocated with an ALLOCATE
         ! statement may be validly passed to DEALLOCATE.
         ! Note:
         ! This error can be returned by STAT in a DEALLOCATE statement.
         ! severe(173): A pointer passed to DEALLOCATE points to an array that cannot be
         ! deallocated
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_SIGSEGV)
         ! One of two possible messages occurs for this error number:
         ! - severe (174): SIGSEGV, segmentation fault occurred
         !   This message indicates that the program attempted an invalid memory
         !   reference. Check the program for possible errors.
         ! - severe (174): SIGSEGV, possible program stack overflow occurred
         !   The following explanatory text also appears:
         !   Program requirements exceed current stacksize resource limit.
         ! severe (174): SIGSEGV, message-text
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_SHORTDATEARG)
         ! The number of characters associated with the DATE argument to the
         ! DATE_AND_TIME intrinsic was shorter than the required length. You must
         ! increase the number of characters passed in for this argument to be at least
         ! eight characters in length. Verify that the TIME and ZONE arguments also meet
         ! their minimum lengths.
         ! severe(175): DATE argument to DATE_AND_TIME is too short (LEN=n), required
         ! LEN=8
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_SHORTTIMEARG)
         ! The number of characters associated with the TIME argument to the
         ! DATE_AND_TIME intrinsic was shorter than the required length. You must
         ! increase the number of characters passed in for this argument to be at least
         ! ten characters in length. Verify that the DATE and ZONE arguments also meet
         ! their minimum lengths.
         ! severe(176): TIME argument to DATE_AND_TIME is too short (LEN=n), required
         ! LEN=10
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_SHORTZONEARG)
         ! The number of characters associated with the ZONE argument to the
         ! DATE_AND_TIME intrinsic was shorter than the required length. You must
         ! increase the number of characters passed in for this argument to be at least
         ! five characters in length. Verify that the DATE and TIME arguments also meet
         ! their minimum lengths.
         ! severe(177): ZONE argument to DATE_AND_TIME is too short (LEN=n), required
         ! LEN=5
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_DIV)
         ! A floating-point or integer divide-by-zero exception occurred.
         ! severe(178): Divide by zero
         throw(EZeroDivide(AMessage, AError))
      case(FOR$IOS_ARRSIZEOVF)
         ! An attempt to dynamically allocate storage for an array failed because the
         ! required storage size exceeds addressable memory.
         ! Note:
         ! This error can be returned by STAT in an ALLOCATE statement.
         ! severe(179): Cannot allocate array - overflow on array size calculation
         throw(EIntOverflow(AMessage, AError))
      case(FOR$IOS_FLTINV_UNINIT)
         ! An invalid floating point operation failed invalid - likely caused by an
         ! uninitialized real/complex variable.
         ! severe (182): floating invalid - possible uninitialized real/complex variable.
         throw(EMathError(AMessage, AError))
      case(183) ! FOR$IOS_NOLIBMEMKINDWARN (no constant available before ifort 17)
         ! An allocation requested FASTMEM but the libmemkind library is not linked into
         ! the executable, so memory will be allocated from the default memory allocator
         ! for that platform.
         ! warning (183): FASTMEM allocation is requested but the libmemkind library is
         ! not linked in, so using the default allocator.
         call logEvent(AMessage)
      case(184) ! FOR$IOS_NOLIBMEMKINDERR (no constant available before ifort 17)
         ! An allocation request for FASTMEM failed because the libmemkind library is not
         ! linked into the executable.
         ! Note:
         ! This error can be returned by STAT in an ALLOCATE statement.
         ! severe (184): FASTMEM allocation is requested but the libmemkind library is
         ! not linked into the executable.
         throw(EExternal(AMessage, AError))
      case(185) ! FOR$IOS_NOFASTMEMWARN (no constant available before ifort 17)
         ! An allocation requested FASTMEM but HBW memory is not available on the node,
         ! so memory will be allocated from the default memory allocator for that
         ! platform.
         ! warning (185): FASTMEM allocation is requested but HBW memory is not
         ! available, so using the default allocator.
         call logEvent(AMessage)
      case(186) ! FOR$IOS_NOFASTMEMERR (no constant available before ifort 17)
         ! An allocation request for FASTMEM failed because HBW memory is not available
         ! on this node.
         ! Note:
         ! This error can be returned by STAT in an ALLOCATE statement.
         ! severe (186): FASTMEM allocation is requested but HBW memory is not available
         ! on this node.
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_RTC_UNINIT_USE_SRC)
         ! The named variable in the named source file is being used without first being
         ! initialized.
         ! severe (194): Run-Time Check Failure. The variable \'%s\' is being used in
         ! \'%s\' without being defined.
         throw(EAssertionFailed(AMessage, AError))
      case(FOR$IOS_UNFIO_FMT)
         ! Attempted unformatted I/O to a unit where the OPEN statement (FORM specifier)
         ! indicated the file was formatted. Check that the correct unit (file) was
         ! specified. If the FORM specifier was not present in the OPEN statement and the
         ! file contains unformatted data, specify FORM='UNFORMATTED' in the OPEN
         ! statement. Otherwise, if appropriate, use formatted I/O (such as list-directed
         ! or namelist I/O).
         ! severe (256): Unformatted I/O to unit open for formatted transfers
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_FMTIO_UNF)
         ! Attempted formatted I/O (such as list-directed or namelist I/O) to a unit
         ! where the OPEN statement indicated the file was unformatted (FORM specifier).
         ! Check that the correct unit (file) was specified. If the FORM specifier was
         ! not present in the OPEN statement and the file contains formatted data,
         ! specify FORM='FORMATTED' in the OPEN statement. Otherwise, if appropriate, use
         ! unformatted I/O.
         ! severe (257): Formatted I/O to unit open for unformatted transfers
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_SEQIO_DIR)
         ! The OPEN statement for this unit number specified direct access and the I/O
         ! statement specifies sequential access. Check the OPEN statement and make sure
         ! the I/O statement uses the correct unit number and type of access.
         ! severe (259): Sequential-access I/O to unit open for direct access
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_OPEREQDIS)
         ! Attempted to use a BACKSPACE statement on such devices as a terminal.
         ! severe (264): operation requires file to be on disk or tape
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_OPEREQSEQ)
         ! Attempted to use a BACKSPACE statement on a file whose organization was not
         ! sequential or whose access was not sequential. A BACKSPACE statement can only
         ! be used for sequential files opened for sequential access.
         ! severe (265): operation requires sequential file organization and access
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_PROABOUSE)
         ! The program called the abort routine to terminate itself.
         ! error (266): Fortran abort routine called
         throw(EAbort(AMessage, AError))
      case(FOR$IOS_ENDRECDUR)
         ! An end-of-record condition was encountered during execution of a non-advancing
         ! I/O READ statement that did not specify the EOR branch specifier.
         ! severe (268): End of record during read
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_FLOINEEXC)
         ! The total number of floating-point inexact data traps encountered during
         ! program execution was nn. This summary message appears at program completion.
         ! info(296): nn floating inexact traps
         call logEvent(AMessage)
      case(FOR$IOS_FLOINVEXC)
         ! The total number of floating-point invalid data traps encountered during
         ! program execution was nn. This summary message appears at program completion.
         ! info (297): nn floating invalid traps
         call logEvent(AMessage)
      case(FOR$IOS_FLOOVFEXC)
         ! The total number of floating-point overflow traps encountered during program
         ! execution was nn. This summary message appears at program completion.
         ! info (298): nn floating overflow traps
         call logEvent(AMessage)
      case(FOR$IOS_FLODIV0EXC)
         ! The total number of floating-point divide-by-zero traps encountered during
         ! program execution was nn. This summary message appears at program completion.
         ! info (299): nn floating divide-by-zero traps
         call logEvent(AMessage)
      case(FOR$IOS_FLOUNDEXC)
         ! The total number of floating-point underflow traps encountered during program
         ! execution was nn. This summary message appears at program completion.
         ! info (300): nn floating underflow traps
         call logEvent(AMessage)
      case(FOR$IOS_INFO_ON_CLOSE)
         ! The unit number specified on a CLOSE statement is not currently open.
         ! Info (529): Attempt to close a unit which was not open.
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6096)
         ! An expression used to index an array was smaller than the lower dimension
         ! bound or larger than the upper dimension bound.
         ! severe (540): Array or substring subscript expression out of range
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_F6097)
         ! An expression used to index a character substring was illegal.
         ! severe (541): CHARACTER substring expression out of range
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_F6098)
         ! The label assigned to the integer-variable name was not specified in the label
         ! list of the assigned GOTO statement.
         ! severe (542): Label not found in assigned GOTO list
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_F6099)
         ! This error occurs whenever integer arithmetic results in overflow.
         ! severe (543): INTEGER arithmetic overflow
         throw(EIntOverflow(AMessage, AError))
      case(FOR$IOS_F6100)
         ! An integer item exceeded the legal size limits.
         ! - An INTEGER (1) item must be in the range -127 to 128.
         ! - An INTEGER (2) item must be in the range -32,767 to 32,768.
         ! - An INTEGER (4) item must be in the range -2,147,483,647 to 2,147,483,648.
         ! severe (544): INTEGER overflow on input
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_F6101)
         ! Either an illegal character appeared as part of an integer, or a numeric
         ! character larger than the radix was used in an alternate radix specifier.
         ! severe (545): Invalid INTEGER
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6102)
         ! An invalid real number was read from a file, an internal variable, or the
         ! console. This can happen if an invalid number is generated by passing an
         ! illegal argument to an intrinsic function -- for example, SQRT(-1) or ASIN(2).
         ! If the invalid result is written and then later read, the error will be
         ! generated.
         ! severe (546): REAL indefinite (uninitialized or previous error)
         throw(EMathError(AMessage, AError))
      case(FOR$IOS_F6103)
         ! An illegal character appeared as part of a real number.
         ! severe (547): Invalid REAL
         throw(EConvertError(AMessage, AError))
      case(FOR$IOS_F6104)
         ! A real value was too large. Floating-point overflows in either direct or
         ! emulated mode generate NaN (Not-A-Number) exceptions, which appear in the
         ! output field as asterisks (*) or the letters NAN.
         ! severe (548): REAL math overflow
         throw(EOverflow(AMessage, AError))
      case(FOR$IOS_F6106)
         ! This error occurs when assignment to an integer is out of range.
         ! severe (550): INTEGER assignment overflow
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_F6200)
         ! The program tried to perform formatted I/O on a unit opened with
         ! FORM='UNFORMATTED' or FORM='BINARY'.
         ! severe (551): Formatted I/O not consistent with OPEN options
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6201)
         ! The program tried to perform list-directed I/O on a file that was not opened
         ! with FORM='FORMATTED' and ACCESS='SEQUENTIAL'.
         ! severe (552): List-directed I/O not consistent with OPEN options
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6202)
         ! When a special device such as CON, LPT1, or PRN is opened in an OPEN
         ! statement, its access must be sequential and its format must be either
         ! formatted or binary. By default ACCESS='SEQUENTIAL' and FORM='FORMATTED' in
         ! OPEN statements.
         ! To generate this error the device's OPEN statement must contain an option not
         ! appropriate for a terminal device, such as ACCESS='DIRECT' or
         ! FORM='UNFORMATTED'.
         ! severe (553): Terminal I/O not consistent with OPEN options
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6203)
         ! A REC= option was included in a statement that transferred data to a file that
         ! was opened with the ACCESS='SEQUENTIAL' option.
         ! severe (554): Direct I/O not consistent with OPEN options
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6204)
         ! If a file is opened with FORM='FORMATTED', unformatted or binary data transfer
         ! is prohibited.
         ! severe (555): Unformatted I/O not consistent with OPEN options
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6205)
         ! The A edit descriptor was not specified when a character data item was read or
         ! written using formatted I/O.
         ! severe (556): A edit descriptor expected for CHARACTER
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6206)
         ! The E, F, D, or G edit descriptor was not specified when a real data item was
         ! read or written using formatted I/O.
         ! severe (557): E, F, D, or G edit descriptor expected for REAL
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6207)
         ! The I edit descriptor was not specified when an integer data item was read or
         ! written using formatted I/O.
         ! severe (558): I edit descriptor expected for INTEGER
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6208)
         ! The L edit descriptor was not specified when a logical data item was read or
         ! written using formatted I/O.
         ! severe (559): L edit descriptor expected for LOGICAL
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6209)
         ! An OPEN statement specified a connection between a unit and a filename that
         ! was already in effect. In this case, only the BLANK= option can have a
         ! different setting.
         ! severe (560): File already open: parameter mismatch
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6210)
         ! The program tried to perform namelist I/O on a file that was not opened with
         ! FORM='FORMATTED' and ACCESS='SEQUENTIAL'.
         ! severe (561): Namelist I/O not consistent with OPEN options
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6211)
         ! IOFOCUS was specified in an OPEN or INQUIRE statement for a non-window unit.
         ! The IOFOCUS option can only be used when the unit opened or inquired about is
         ! a QuickWin child window.
         ! severe (562): IOFOCUS option illegal with non-window unit
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6212)
         ! IOFOCUS was specified in an OPEN or INQUIRE statement for a non-QuickWin
         ! application. The IOFOCUS option can only be used when the unit opened or
         ! inquired about is a QuickWin child window.
         ! severe (563): IOFOCUS option illegal without QuickWin
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6213)
         ! TITLE was specified in an OPEN or INQUIRE statement for a non-window unit. The
         ! TITLE option can only be used when the unit opened or inquired about is a
         ! QuickWin child window.
         ! severe (564): TITLE illegal with non-window unit
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6214)
         ! TITLE was specified in an OPEN or INQUIRE statement for a non-QuickWin
         ! application. The TITLE option can only be used when the unit opened or
         ! inquired about is a QuickWin child window.
         ! severe (565): TITLE illegal without QuickWin
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6300)
         ! STATUS='KEEP' was specified for a scratch file; this is illegal because
         ! scratch files are automatically deleted at program termination.
         ! severe (566): KEEP illegal for scratch file
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6301)
         ! STATUS='SCRATCH' should not be used in a statement that includes a filename.
         ! severe (567): SCRATCH illegal for named file
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6302)
         ! More than one alternate radix for numeric I/O was specified. F6302 can
         ! indicate an error in spacing or a mismatched format for data of different
         ! radices.
         ! severe (568): Multiple radix specifiers
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6303)
         ! A radix specifier was not between 2 and 36, inclusive. Alternate radix
         ! constants must be of the form n#ddd... where n is a radix from 2 to 36
         ! inclusive and ddd... are digits with values less than the radix. For example,
         ! 3#12 and 34#7AX are valid constants with valid radix specifiers. 245#7A and
         ! 39#12 do not have valid radix specifiers and generate error 569 if input.
         ! severe (569): Illegal radix specifier
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6304)
         ! An illegal value was used with the STATUS option.
         ! STATUS accepts the following values:
         ! - 'KEEP' or 'DELETE'' when used with CLOSE statements
         ! - 'OLD', 'NEW', 'SCRATCH', or 'UNKNOWN' when used with OPEN statements
         ! severe (570): Illegal STATUS value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6305)
         ! An illegal value was used with the MODE option.
         ! MODE accepts the values 'READ', 'WRITE', or 'READWRITE'.
         ! severe (571): Illegal MODE value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6306)
         ! An illegal value was used with the ACCESS option.
         ! ACCESS accepts the values 'SEQUENTIAL' and 'DIRECT'.
         ! severe (572): Illegal ACCESS value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6307)
         ! An illegal value was used with the BLANK option.
         ! BLANK accepts the values 'NULL' and 'ZERO'.
         ! severe (573): Illegal BLANK value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6308)
         ! An illegal value was used with the FORM option.
         ! FORM accepts the following values: 'FORMATTED', 'UNFORMATTED', and 'BINARY'.
         ! severe (574): Illegal FORM value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6309)
         ! An illegal value was used with the SHARE option.
         ! SHARE accepts the values 'COMPAT', 'DENYRW', 'DENYWR', 'DENYRD', and
         ! 'DENYNONE'.
         ! severe (575): Illegal SHARE value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6311)
         ! An invalid number was specified as the record number for a direct-access file.
         ! The first valid record number for direct-access files is 1.
         ! severe (577): Illegal record number
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6312)
         ! In an INQUIRE statement, the NUMBER option was specified for the file
         ! associated with * (console).
         ! severe (578): No unit number associated with *
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6314)
         ! An illegal unit number was specified.
         ! Legal unit numbers can range from 0 through 2**31-1, inclusive.
         ! severe (580): Illegal unit number
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6315)
         ! A negative or zero record length was specified for a direct file.
         ! The smallest valid record length for direct files is 1.
         ! severe (581): Illegal RECL value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6316)
         ! The program attempted to ALLOCATE an already allocated array.
         ! severe (582): Array already allocated
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6317)
         ! The size specified for an array in an ALLOCATE statement must be greater than
         ! zero.
         ! severe (583): Array size zero or negative
         throw(EInvalidOpException(AMessage, AError))
      !case(FOR$IOS_F6318)
         ! severe (584): Non-HUGE array exceeds 64K
         ! This code is irrelevant, the current memory models talk about 2G
      case(FOR$IOS_F6319)
         ! The program attempted to DEALLOCATE an array that was never allocated.
         ! severe (585): Array not allocated
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6400)
         ! A BACKSPACE statement specified a unit connected to a terminal device such as
         ! a terminal or printer.
         ! severe (586): BACKSPACE illegal on terminal device
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6401)
         ! An EOF intrinsic function specified a unit connected to a terminal device such
         ! as a terminal or printer.
         ! severe (587): EOF illegal on terminal device
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6402)
         ! An ENDFILE statement specified a unit connected to a terminal device such as a
         ! terminal or printer.
         ! severe (588): ENDFILE illegal on terminal device
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6403)
         ! A REWIND statement specified a unit connected to a terminal device such as a
         ! terminal or printer.
         ! severe (589): REWIND illegal on terminal device
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6404)
         ! A CLOSE statement specified STATUS='DELETE' for a read-only file.
         ! severe (590): DELETE illegal for read-only file
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6405)
         ! The program tried to access a file after executing an ENDFILE statement or
         ! after it encountered the end-of-file record during a read operation.
         ! A BACKSPACE, REWIND, or OPEN statement must be used to reposition the file
         ! before execution of any I/O statement that transfers data.
         ! severe (591): External I/O illegal beyond end of file
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6406)
         ! severe (592): Truncation error: file closed
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6407)
         ! More than 131 characters were input to a record of a unit connected to the
         ! terminal (keyboard). Note that the operating system may impose additional
         ! limits on the number of characters that can be input to the terminal in a
         ! single record.
         ! severe (593): Terminal buffer overflow
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6408)
         ! If you have record lengths that exceed the buffer size associated with the
         ! record, (for instance, the record is a file with the buffer set by BLOCKSIZE
         ! in the OPEN statement), either you should not do left tabbing within the
         ! record, or you should not use commas as field delimiters. This is because
         ! commas are disabled as input field delimiters if left tabbing leaves the
         ! record positioned in a previous buffer.
         ! For example, consider you have a file LONG.DAT that is one continuous record
         ! with data fields separated by commas. You then set the buffer associated with
         ! the file to 512 bytes, read more than one buffer size of data, tab left to
         ! data in the previous buffer, and attempt to read further data, as follows:
         !     INTEGER value(300)
         !    OPEN (1, FILE = 'LONG.DAT', BLOCKSIZE = 512)s
         !    READ (1, 100) (value(i), i = 1, 300)s
         ! 100 FORMAT (290I2,TL50,10I2)
         ! In this case, error 594 occurs.
         ! severe (594): Comma delimiter disabled after left repositioning
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6413)
         ! The program tried to connect an already connected file to a new unit.
         ! A file can be connected to only one unit at a time.
         ! severe (599): File already connected to a different unit
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6414)
         ! This error can be caused by one of the following:
         ! - The filename specified in an OPEN statement was a directory.
         ! - An OPEN statement tried to open a read-only file for writing.
         ! - The file was opened with SHARE='DENYRW' by another process.
         ! severe (600): Access not allowed
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6415)
         ! An OPEN statement specified STATUS='NEW' for a file that already exists.
         ! severe (601): File already exists
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6416)
         ! An OPEN statement specified STATUS='OLD' for a specified file or a directory
         ! path that does not exist.
         ! severe (602): File not found
         throw(EFileNotFoundException(AMessage, AError))
      case(FOR$IOS_F6417)
         ! The program exceeded the number of open files the operating system allows.
         ! severe (603): Too many open files
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_F6418)
         ! The program exceeded the number of units that can be connected at one time.
         ! Units are connected with the OPEN statement.
         ! severe (604): Too many units connected
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_F6419)
         ! The file was opened with FORM='UNFORMATTED' and ACCESS='SEQUENTIAL', but its
         ! internal physical structure was incorrect or inconsistent. Possible causes:
         ! the file was created in another mode or by a non-Fortran program.
         ! severe (605): Illegal structure for unformatted file
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6420)
         ! A statement such as BACKSPACE or ENDFILE specified a file that had not yet
         ! been opened. (The READ and WRITE statements do not cause this problem because
         ! they prompt you for a file if the file has not been opened yet.)
         ! severe (606): Unknown unit number
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6421)
         ! The program tried to transfer data to a file that was opened in read-only mode
         ! or locked against writing.
         ! The error message may indicate a CLOSE error when the fault is actually coming
         ! from WRITE. This is because the error is not discovered until the program
         ! tries to write buffered data when it closes the file.
         ! severe (607): File read-only or locked against writing
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6422)
         ! The program tried to transfer data to a file residing on a device (such as a
         ! hard disk) that was out of storage space.
         ! severe (608): No space left on device
         throw(EInOutError(AMessage, AError))
      !case(FOR$IOS_F6423)
         ! Too many threads were active simultaneously. At most, 32 threads can be active
         ! at one time. Close any unnecessary processes or child windows within your
         ! application.
         ! severe (609): Too many threads
         ! This code is irrelevant, the operating system might define an upper bound, but
         ! this is nowhere near 32.
      case(FOR$IOS_F6424)
         ! severe (610): Invalid argument
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6425)
         ! The BACKSPACE statement is not allowed in files opened with MODE='WRITE'
         ! (write-only status) because BACKSPACE requires reading the previous record in
         ! the file to provide positioning.
         ! Resolve the problem by giving the file read access or by avoiding the
         ! BACKSPACE statement. Note that the REWIND statement is valid for files opened
         ! as write-only.
         ! severe (611): BACKSPACE illegal for SEQUENTIAL write-only files
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6500)
         ! The program tried to read from a file that was not opened for reading or was
         ! locked.
         ! severe (612): File not open for reading or file locked
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6501)
         ! The program tried to read more data than the file contains.
         ! severe (613): End of file encountered
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6502)
         ! When the i*c form is used in list-directed input, the i must be a positive
         ! integer. For example, consider the following statement:
         !   READ(*,*) a, b
         ! Input 2*56.7 is accepted, but input 2.1*56.7 returns error 614.
         ! severe (614): Positive integer expected in repeat field
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6503)
         ! In list-directed input of the form i*c, an extra repeat field was used. For
         ! example, consider the following:
         !   READ(*,*) I, J, K
         ! Input of 2*1*3 returns this error. The 2*1 means send two values, each 1; the
         ! *3 is an error.
         ! severe (615): Multiple repeat field
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6504)
         ! Some of the values in a list-directed input record were not numeric. For
         ! example, consider the following:
         !   READ(*,*) I, J
         ! The preceding statement would cause this error if the input were: 123 'abc'.
         ! severe (616): Invalid number in input
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6505)
         ! A string item was not enclosed in single quotation marks.
         ! severe (617): Invalid string in input
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6506)
         ! When using list-directed input, the real and imaginary components of a complex
         ! number were not separated by a comma.
         ! severe (618): Comma missing in COMPLEX input
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6507)
         ! The wrong format was used for the input field for logical data.
         ! The input field for logical data consists of optional blanks, followed by an
         ! optional decimal point, followed by a T for true or F for false. The T or F
         ! may be followed by additional characters in the field, so that .TRUE. and
         ! .FALSE. are acceptable input forms.
         ! severe (619): T or F expected in LOGICAL read
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6508)
         ! The program tried to read more data from an unformatted file than the current
         ! record contained. If the program was reading from an unformatted direct file,
         ! it tried to read more than the fixed record length as specified by the RECL
         ! option. If the program was reading from an unformatted sequential file, it
         ! tried to read more data than was written to the record.
         ! severe (620): Too many bytes read from unformatted record
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6509)
         ! Hollerith (H) or apostrophe edit descriptors were encountered in a format used
         ! by a READ statement.
         ! severe (621): H or apostrophe edit descriptor illegal on input
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6510)
         ! The input field contained a character that was not hexadecimal.
         ! Legal hexadecimal characters are 0 - 9 and A - F.
         ! severe (622): Illegal character in hexadecimal input
         throw(EConvertError(AMessage, AError))
      case(FOR$IOS_F6511)
         ! A name encountered on input from a namelist record is not declared in the
         ! corresponding NAMELIST statement.
         ! severe (623): Variable name not found
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_F6512)
         ! The input record is not in the correct form for NAMELIST input.
         ! severe (624): Invalid NAMELIST input format
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_F6513)
         ! In NAMELIST input, an array name was qualified with a different number of
         ! subscripts than its declaration, or a non-array name was qualified.
         ! severe (625): Wrong number of array dimensions
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_F6514)
         ! A subscript was specified in NAMELIST input which exceeded the declared
         ! dimensions of the array.
         ! severe (626): Array subscript exceeds allocated area
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_F6515)
         ! A character item in namelist input was qualified with a subrange that did not
         ! meet the requirement that 1 <= e1 <= e2 <= len (where "len" is the length of
         ! the character item, "e1" is the leftmost position of the substring, and "e2"
         ! is the rightmost position of the substring).
         ! severe (627): Invalid subrange in NAMELIST input
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_F6516)
         ! A non-CHARACTER item in namelist input was qualified with a substring range.
         ! severe (628): Substring range specified on non-CHARACTER item
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_F6600)
         ! The program either overflowed an internal-file record or tried to write to a
         ! record beyond the end of an internal file.
         ! severe (629): Internal file overflow
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6601)
         ! The program tried to write more than the number of bytes specified in the RECL
         ! option to an individual record of a direct-access file.
         ! severe (630): Direct record overflow
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6602)
         ! The program tried to write a non-CHARACTER item across a record boundary in
         ! list-directed or namelist output. Only character constants can cross record
         ! boundaries.
         ! severe (631): Numeric field bigger than record size
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6700)
         ! The program ran out of heap space. The ALLOCATE statement and various internal
         ! functions allocate memory from the heap. This error will be generated when the
         ! last of the heap space is used up.
         ! severe (632): Heap space limit exceeded
         call Exception$throwHeapException(HeapExceptions$IOS_F6700)
      case(FOR$IOS_F6701)
         ! The program exhausted the template used to generate unique scratch-file names.
         ! The maximum number of scratch files that can be open at one time is 26.
         ! severe (633): Scratch file name limit exceeded
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6970)
         ! The specified decimal length d exceeds the specified total field width w in an
         ! ES edit descriptor.
         ! severe (634): D field exceeds W field in ES edit descriptor
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6971)
         ! The specified decimal length d exceeds the specified total field width w in an
         ! EN edit descriptor.
         ! severe (635): D field exceeds W field in EN edit descriptor
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6972)
         ! severe (636): Exponent of 0 not allowed in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6980)
         ! An edit descriptor lacked a required integer value. For example, consider the
         ! following:
         !       WRITE(*, 100) I, J
         !   100 FORMAT (I2, TL, I2)
         ! The preceding code will cause this error because an integer is expected after
         ! TL.
         ! severe (637): Integer expected in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6981)
         ! A format did not begin with a left parenthesis ( ( ).
         ! severe (638): Initial left parenthesis expected in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6982)
         ! A zero or negative integer value was used in a format.
         ! Negative integer values can appear only with the P edit descriptor. Integer
         ! values of 0 can appear only in the d and m fields of numeric edit descriptors.
         ! severe (639): Positive integer expected in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6983)
         ! One or more BN, BZ, S, SS, SP, T, TL, TR, /, $, :, or apostrophe (') edit
         ! descriptors had repeat counts associated with them.
         ! severe (640): Repeat count on nonrepeatable descriptor
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6984)
         ! An integer did not precede a (nonrepeatable) H, X, or P edit descriptor.
         ! The correct formats for these descriptors are nH, nX, and kP, respectively,
         ! where n is a positive integer and k is an optionally signed integer.
         ! severe (641): Integer expected preceding H, X, or P edit descriptor
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6985)
         ! To control interpretation of embedded and trailing blanks within numeric input
         ! fields, you must specify BN (to ignore them) or BZ (to interpret them as
         ! zeros).
         ! severe (642): N or Z expected after B in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6986)
         ! More than sixteen sets of parentheses were nested inside the main level of
         ! parentheses in a format.
         ! severe (643): Format nesting limit exceeded
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6987)
         ! No period appeared between the w and d fields of a D, E, F, or G edit
         ! descriptor.
         ! severe (644): '.' expected in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6988)
         ! An incomplete format was used.
         ! Improperly matched parentheses, an unfinished Hollerith (H) descriptor, or
         ! another incomplete descriptor specification can cause this error.
         ! severe (645): Unexpected end of format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6989)
         ! A character that cannot be interpreted as part of a valid edit descriptor was
         ! used in a format. For example, consider the following:
         !       WRITE(*, 100) I, J
         !   100 FORMAT (I2, TL4.5, I2)
         ! The code will generate this error because TL4.5 is not a valid edit
         ! descriptor. An integer must follow TL.
         ! severe (646): Unexpected character in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6990)
         ! In syntax Iw.m, the value of m cannot exceed the value of w.
         ! severe (647): M field exceeds W field in I edit descriptor
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6991)
         ! An integer value specified in an edit descriptor was too large to represent as
         ! a 4-byte integer.
         ! severe (648): Integer out of range in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6992)
         ! The format specifier in a READ, WRITE, or PRINT statement was an integer
         ! variable, but an ASSIGN statement did not properly assign it the statement
         ! label of a FORMAT statement in the same program unit.
         ! severe (649): format not set by ASSIGN
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6993)
         ! Within format specifications, edit descriptors must be separated by commas or
         ! slashes (/).
         ! severe (650): Separator expected in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6994)
         ! severe (651): %c or $: nonstandard edit descriptor in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6995)
         ! Z is not a standard edit descriptor in format.
         ! If you want to transfer hexadecimal values, you must use the edit descriptor
         ! form Zw[.m], where w is the field width and m is the minimum number of digits
         ! that must be in the field (including leading zeros).
         ! severe (652): Z: nonstandard edit descriptor in format
         throw(EFormatError(AMessage, AError))
      case(FOR$IOS_F6996)
         ! severe (653): DOS graphics not supported under Windows NT
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_F6997)
         ! An OPEN statement in which IOFOCUS was TRUE, either explicitly or by default,
         ! failed because the new window could not receive focus. The window handle may
         ! be invalid, or closed, or there may be a memory resource problem.
         ! severe (654): Graphics error
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_F6998)
         ! A call to QuickWin from a console application was encountered during
         ! execution.
         ! severe (655): Using QuickWin is illegal in console application
         throw(EExternal(AMessage, AError))
      case(FOR$IOS_F6999)
         ! The ADVANCE option can only take the values 'YES' and 'NO'. ADVANCE='YES' is
         ! the default. ADVANCE is a READ statement option.
         ! severe (656): Illegal 'ADVANCE' value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6702)
         ! The argument specified for DIM must be greater than or equal to 1, and less
         ! than or equal to the number of dimensions in the specified array. Consider the
         ! following:
         !   i = SIZE (array, DIM = dim)
         ! In this case, 1 <= dim <= n, where n is the number of dimensions in array.
         ! severe (657): DIM argument to SIZE out of range
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6703)
         ! A POINTER used as an argument to the ASSOCIATED function must be defined; that
         ! is, assigned to a target, allocated, or nullified.
         ! severe (658): Undefined POINTER used as argument to ASSOCIATED function
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6704)
         ! Except in an assignment statement, a pointer must not be referenced until it
         ! has been initialized: assigned to a target, allocated or nullified.
         ! severe (659): Reference to uninitialized POINTER
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6705)
         ! Except in an assignment statement and certain procedure references, a pointer
         ! must not be referenced until it has been associated: either assigned to a
         ! target or allocated.
         ! severe (660): Reference to POINTER which is not associated
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6706)
         ! Except in an assignment statement, a pointer must not be referenced until it
         ! has been initialized: assigned to a target, allocated or nullified.
         ! severe (661): Reference to uninitialized POINTER 'pointer'
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6707)
         ! Except in an assignment statement and certain procedure references, a pointer
         ! must not be referenced until it has been associated: either assigned to a
         ! target or allocated.
         ! severe (662): reference to POINTER 'pointer' which is not associated
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6708)
         ! A substring starting position must be a positive integer variable or
         ! expression that indicates a position in the string: at least 1 and no greater
         ! than the length of the string.
         ! severe (663): Out of range: substring starting position 'pos' is less than 1
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_F6709)
         ! A substring ending position must be a positive integer variable or expression
         ! that indicates a position in the string: at least 1 and no greater than the
         ! length of the string.
         ! severe (664): Out of range: substring ending position 'pos' is greater than
         ! string length 'len'
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_F6710)
         ! The subscript for a substring within a string is not a valid string position:
         ! at least 1 and no greater than the length of the string.
         ! severe (665): Subscript 'n' of 'str' (value 'val') is out of range
         ! ('first:last')
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_F6711)
         ! The subscript for a substring within a string is not a valid string position:
         ! at least 1 and no greater than the length of the string.
         ! severe (666): Subscript 'n' of 'str' (value 'val') is out of range ('first:*')
         throw(ERangeError(AMessage, AError))
      case(FOR$IOS_F6712)
         ! The character length of elements in the VECTOR argument to PACK is not the
         ! same as the character length of elements in the array to be packed.
         ! severe (667): VECTOR argument to PACK has incompatible character length
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6713)
         ! The VECTOR argument to PACK must have at least as many elements as there are
         ! true elements in MASK (the array that controls packing).
         ! severe (668): VECTOR argument to PACK is too small
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6714)
         ! The character length of elements in the SOURCE and PAD arguments to PACK must
         ! be the same.
         ! severe (669): SOURCE and PAD arguments to RESHAPE have different character
         ! lengths
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6715)
         ! The SHAPE vector specifies the shape of the reshaped array. Since an array
         ! cannot have a negative dimension, SHAPE cannot have a negative element.
         ! severe (670): Element 'n' of SHAPE argument to RESHAPE is negative
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6716)
         ! If there is no PAD array, the SOURCE argument to RESHAPE must have enough
         ! elements to make an array of the shape specified by SHAPE.
         ! severe (671): SOURCE too small for specified SHAPE in RESHAPE, and no PAD
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6717)
         ! The system ran out of memory while trying to make the array specified by
         ! RESHAPE. If possible, reset your virtual memory size through the Windows*
         ! Control Panel, or close unnecessary applications and deallocate all allocated
         ! arrays that are no longer needed.
         ! severe (672): Out of memory
         call Exception$throwHeapException(HeapExceptions$IOS_F6717)
      case(FOR$IOS_F6718)
         ! ORDER specifies the order of the array dimensions given in SHAPE, and they
         ! must be vectors of the same size.
         ! severe (673): SHAPE and ORDER arguments to RESHAPE have different sizes
         ! ('size1' and 'size2')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6719)
         ! The ORDER argument specifies the order of the dimensions of the reshaped
         ! array, and it must be a permuted list of (1, 2, ..., n) where n is the highest
         ! dimension in the reshaped array.
         ! severe (674): Element 'n' of ORDER argument to RESHAPE is out of range
         ! ('range')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6720)
         ! The ORDER vector specifies the order of the dimensions of the reshaped array,
         ! and it must be a permuted list of (1, 2, ..., n) where n is the highest
         ! dimension in the reshaped array. No dimension can occur twice.
         ! severe (675): Value 'val' occurs twice in ORDER argument to RESHAPE
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6721)
         ! severe (676): Impossible nextelt overflow in RESHAPE
         throw(Exception(AMessage, AError))
         ! TODO: What is this?
      case(FOR$IOS_F6722)
         ! The argument specified for DIM to SPREAD must be greater than or equal to 1,
         ! and less than or equal to one larger than the number of dimensions (rank) of
         ! SOURCE. Consider the following statement:
         !   result = SPREAD (SOURCE= array, DIM = dim, NCOPIES = k)
         ! In this case, 1 <= dim <= n+ 1, where nis the number of dimensions in array.
         ! severe (677): Invalid value 'dim' for argument DIM for SPREAD of rank 'rank'
         ! source
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6723)
         ! Zero of any type (complex, real, or integer) cannot be raised to zero power.
         ! severe (678): Complex zero raised to power zero
         throw(EMathError(AMessage, AError))
      case(FOR$IOS_F6724)
         ! Zero of any type (complex, real, or integer) cannot be raised to a negative
         ! power. Raising to a negative power inverts the operand.
         ! severe (679): Complex zero raised to negative power
         throw(EMathError(AMessage, AError))
      case(FOR$IOS_F6725)
         ! severe (680): Impossible error in NAMELIST input
         throw(ENameListError(AMessage, AError))
      case(FOR$IOS_F6726)
         ! The optional argument DIM specifies the dimension along which to perform the
         ! circular shift, and must be greater than or equal to 1 and less than or equal
         ! to the number of dimensions in the array to be shifted. That is,
         ! 1 <= DIM <= n, where nis the number of dimensions in the array to be shifted.
         ! severe (681): DIM argument to CSHIFT ('dim') is out of range
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6727)
         ! The optional argument DIM specifies the dimension along which to perform the
         ! circular shift, and must be greater than or equal to 1 and less than or equal
         ! to the number of dimensions in the array to be shifted. That is,
         ! 1 <= DIM <= n, where nis the number of dimensions in the array to be shifted.
         ! severe (682): DIM argument ('dim') to CSHIFT is out of range (1:'n')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6728)
         ! The SHIFT argument to CSHIFT must be either scalar or an array one dimension
         ! smaller than the shifted array. If an array, the shape of the SHIFT must
         ! conform to the shape of the array being shifted in every dimension except the
         ! one being shifted along.
         ! severe (683): Shape mismatch (dimension 'dim') between ARRAY and SHIFT in
         ! CSHIFT
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6729)
         ! severe (684): Internal error - bad arguments to CSHIFT_CA
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6730)
         ! severe (685): Internal error - bad arguments to CSHIFT_CAA
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6731)
         ! The character DATE argument must have a length of at least eight to contain
         ! the complete value.
         ! severe (686): DATE argument to DATE_AND_TIME is too short (LEN='len')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6732)
         ! The character TIME argument must have a length of at least ten to contain the
         ! complete value.
         ! severe (687): TIME argument to DATE_AND_TIME is too short (LEN='len')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6733)
         ! The character ZONE argument must have a length of at least five to contain the
         ! complete value.
         ! severe (688): ZONE argument to DATE_AND_TIME is too short (LEN='len')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6734)
         ! The integer VALUES argument must be a one-dimensional array with a size of at
         ! least eight to hold all returned values.
         ! severe (689): VALUES argument to DATE_AND_TIME is too small ('size' elements)
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6735)
         ! The optional argument DIM specifies the dimension along which to count true
         ! elements of MASK, and must be greater than or equal to 1 and less than or
         ! equal to the number of dimensions in MASK. That is, 1 <= DIM <= n, where n is
         ! the number of dimensions in MASK.
         ! severe (690): Out of range: DIM argument to COUNT has value 'dim'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6736)
         ! The optional argument DIM specifies the dimension along which to count true
         ! elements of MASK, and must be greater than or equal to 1 and less than or
         ! equal to the number of dimensions (rank) in MASK. That is, 1 <= DIM <= n,
         ! where nis the number of dimensions in MASK.
         ! severe (691): Out of range: DIM argument to COUNT has value 'dim' with MASK of
         ! rank 'rank'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6737)
         ! The optional argument DIM specifies the dimension along which to compute the
         ! product of elements in an array, and must be greater than or equal to 1 and
         ! less than or equal to the number of dimensions in the array. That is,
         ! 1 <= DIM <= n, where nis the number of dimensions in array holding the
         ! elements to be multiplied.
         ! severe (692): Out of range: DIM argument to PRODUCT has value 'dim'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6738)
         ! The optional argument DIM specifies the dimension along which to compute the
         ! product of elements in an array, and must be greater than or equal to 1 and
         ! less than or equal to the number of dimensions (rank) of the array. That is,
         ! 1 <= DIM <= n, where nis the number of dimensions in array holding the
         ! elements to be multiplied.
         ! severe (693): Out of range: DIM argument to PRODUCT has value 'dim' with ARRAY
         ! of rank 'rank'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6739)
         ! The optional argument DIM specifies the dimension along which to sum the
         ! elements of an array, and must be greater than or equal to 1 and less than or
         ! equal to the number of dimensions (rank) of the array. That is, 1 <= DIM <= n,
         ! where nis the number of dimensions in array holding the elements to be summed.
         ! severe (694): Out of range: DIM argument to SUM has value 'dim' with ARRAY of
         ! rank 'rank'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6740)
         ! Zero of any type (real, complex, or integer) cannot be raised to zero power.
         ! severe (695): Real zero raised to zero power
         throw(EMathError(AMessage, AError))
      case(FOR$IOS_F6741)
         ! Zero of any type (real, complex, or integer) cannot be raised to a negative
         ! power. Raising to a negative power inverts the operand.
         ! severe (696): Real zero raised to negative power
         throw(EMathError(AMessage, AError))
      case(FOR$IOS_F6742)
         ! The optional argument DIM specifies the dimension along which to sum the
         ! elements of an array, and must be greater than or equal to 1 and less than or
         ! equal to the number of dimensions in the array. That is, 1 <= DIM <= n, where
         ! nis the number of dimensions in array holding the elements to be summed.
         ! severe (697): Out of range: DIM argument to SUM has value 'dim'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6743)
         ! The optional argument DIM specifies the dimension along which to perform an
         ! end-off shift in an array, and must be greater than or equal to 1 and less
         ! than or equal to the number of dimensions in the array. That is,
         ! 1 <= DIM <= n, where nis the number of dimensions in array holding the
         ! elements to be shifted.
         ! severe (698): DIM argument ('dim') to EOSHIFT is out of range (1:'n')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6744)
         ! The BOUNDARY argument to EOSHIFT must be either scalar or an array one
         ! dimension smaller than the shifted array. If an array, the shape of the
         ! BOUNDARY must conform to the shape of the array being shifted in every
         ! dimension except the one being shifted along.
         ! severe (699): Shape mismatch (dimension 'dim') between ARRAY and BOUNDARY in
         ! EOSHIFT
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6745)
         ! The optional argument DIM specifies the dimension along which to perform an
         ! end-off shift in an array, and must be greater than or equal to 1 and less
         ! than or equal to the number of dimensions in the array. That is,
         ! 1 <= DIM <= n, where nis the number of dimensions in array holding the
         ! elements to be shifted.
         ! severe (700): DIM argument to EOSHIFT is out of range ('dim')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6746)
         ! The SHIFT argument to EOSHIFT must be either scalar or an array one dimension
         ! smaller than the shifted array. If an array, the shape of the SHIFT must
         ! conform to the shape of the array being shifted in every dimension except the
         ! one being shifted along.
         ! severe (701): Shape mismatch (dimension 'dim') between ARRAY and SHIFT in
         ! EOSHIFT
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6747)
         ! The character length of elements in the BOUNDARY argument and in the array
         ! being end-off shifted must be the same.
         ! severe (702): BOUNDARY argument to EOSHIFT has wrong LEN ('len1 instead of
         ! len2')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6748)
         ! severe (703): BOUNDARY has LEN 'len' instead of 'len' to EOSHIFT
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6749)
         ! severe (704): Internal error - bad arguments to EOSHIFT
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6750)
         ! The value used for the number of the command-line argument to retrieve with
         ! GETARG must be 0 or a positive integer. If the number of the argument to be
         ! retrieved is greater than the actual number of arguments, blanks are returned,
         ! but no error occurs.
         ! severe (705): GETARG: value of argument 'num' is out of range
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6751)
         ! The unit number specifying which I/O unit to flush to its associated file must
         ! be an integer between 0 and 2**31-1, inclusive. If the unit number is valid,
         ! but the unit is not opened, error F6752 is generated.
         ! severe (706): FLUSH: value of LUNIT 'num' is out of range
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6752)
         ! The I/O unit specified to be flushed to its associated file is not connected
         ! to a file.
         ! severe (707): FLUSH: Unit 'n' is not connected
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6753)
         ! The character argument to ICHAR must have length of one.
         ! severe (708): Invalid string length ('len') to ICHAR
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6754)
         ! The character argument to IACHAR must have length of one.
         ! severe (709): Invalid string length ('len') to IACHAR
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6755)
         ! Zero of any type (integer, real, or complex) cannot be raised to a negative
         ! power. Raising to a negative power inverts the operand.
         ! severe (710): Integer zero raised to negative power
         throw(EDivByZero(AMessage, AError))
      case(FOR$IOS_F6756)
         ! Zero of any type (integer, real, or complex) cannot be raised to zero power.
         ! severe (711): INTEGER zero raised to zero power
         throw(EIntError(AMessage, AError))
      case(FOR$IOS_F6757)
         ! The argument SIZE must be positive and must not exceed the bit size of the
         ! integer being shifted. The bit size of this integer can be determined with the
         ! function BIT_SIZE.
         ! severe (712): SIZE argument ('size') to ISHFTC intrinsic out of range
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6758)
         ! The argument SHIFT to ISHFTC must be an integer whose absolute value is less
         ! than or equal to the number of bits being shifted: either all bits in the
         ! number being shifted or a subset specified by the optional argument SIZE.
         ! severe (713): SHIFT argument ('shift') to ISHFTC intrinsic out of range
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6759)
         ! The optional argument DIM specifies the dimension whose lower bound is to be
         ! returned, and must be greater than or equal to 1 and less than or equal to the
         ! number of dimensions in the array. That is, 1 <= DIM <= n, where n is the
         ! number of dimensions in array.
         ! severe (714): Out of range: DIM argument to LBOUND has value 'dim'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6760)
         ! The optional argument DIM specifies the dimension whose lower bound is to be
         ! returned, and must be greater than or equal to 1 and less than or equal to the
         ! number of dimensions (rank) in the array. That is, 1 <= DIM <= n, where n is
         ! the number of dimensions in array.
         ! severe (715): Out of range: DIM argument ('dim') to LBOUND greater than ARRAY
         ! rank 'rank'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6761)
         ! The optional argument DIM specifies the dimension along which maximum values
         ! are returned, and must be greater than or equal to 1 and less than or equal to
         ! the number of dimensions in the array. That is, 1 <= DIM <= n, where n is the
         ! number of dimensions in array.
         ! severe (716): Out of range: DIM argument to MAXVAL has value 'dim'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6762)
         ! The optional argument DIM specifies the dimension along which maximum values
         ! are returned, and must be greater than or equal to 1 and less than or equal to
         ! the number of dimensions (rank) in the array. That is, 1 <= DIM <= n, where
         ! n is the number of dimensions in array.
         ! severe (717): Out of range: DIM argument to MAXVAL has value 'dim' with ARRAY
         ! of rank 'rank'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6763)
         ! There is not enough memory space to hold a temporary array.
         ! Dynamic memory allocation is limited by several factors, including swap file
         ! size and memory requirements of other applications that are running. If you
         ! encounter an unexpectedly low limit, you might need to reset your virtual
         ! memory size through the Windows Control Panel or redefine the swap file size.
         ! Allocated arrays that are no longer needed should be deallocated.
         ! severe (718): Cannot allocate temporary array -- out of memory
         call Exception$throwHeapException(HeapExceptions$IOS_F6763)
      case(FOR$IOS_F6764)
         ! An attempt was made to DEALLOCATE a pointer to an array subsection or an
         ! element within a derived type. The whole data object must be deallocated;
         ! parts cannot be deallocated.
         ! severe (719): Attempt to DEALLOCATE part of a larger object
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6765)
         ! Deallocating a pointer associated with an allocatable target is illegal.
         ! Instead, deallocate the target the pointer points to, which frees memory and
         ! disassociates the pointer.
         ! severe (720): Pointer in DEALLOCATE is ASSOCIATED with an ALLOCATABLE array
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6766)
         ! You cannot deallocate an array unless it has been previously allocated. You
         ! cannot deallocate a pointer whose target was not created by allocation. The
         ! intrinsic function ALLOCATED can be used to determine whether an allocatable
         ! array is currently allocated.
         ! severe (721): Attempt to DEALLOCATE an object which was not allocated
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6767)
         ! There is not enough memory space to allocate the pointer.
         ! Dynamic memory allocation is limited by several factors, including swap file
         ! size and memory requirements of other applications that are running. If you
         ! encounter an unexpectedly low limit, you might need to reset your virtual
         ! memory size through the Windows* Control Panel or redefine the swap file size.
         ! Allocated arrays that are no longer needed should be deallocated.
         ! severe (722): Cannot ALLOCATE scalar POINTER -- out of memory
         call Exception$throwHeapException(HeapExceptions$IOS_F6767)
      case(FOR$IOS_F6768)
         ! You cannot deallocate an array unless it has been previously allocated. You
         ! cannot deallocate a pointer whose target was not created by allocation, or a
         ! pointer that has undefined association status.
         ! The intrinsic function ALLOCATED can be used to determine whether an
         ! allocatable array is currently allocated.
         ! severe (723): DEALLOCATE: object not allocated/associated
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6769)
         ! There is not enough memory space to allocate the POINTER array.
         ! Dynamic memory allocation is limited by several factors, including swap file
         ! size and memory requirements of other applications that are running. If you
         ! encounter an unexpectedly low limit, you might need to reset your virtual
         ! memory size through the Windows* Control Panel or redefine the swap file size.
         ! Allocated arrays that are no longer needed should be deallocated.
         ! severe (724): Cannot ALLOCATE POINTER array -- out of memory
         call Exception$throwHeapException(HeapExceptions$IOS_F6769)
      case(FOR$IOS_F6770)
         ! It is illegal to DEALLOCATE an array that is not allocated. You can check the
         ! allocation status of an array before deallocating with the ALLOCATED function.
         ! severe (725): DEALLOCATE: Array not allocated
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6771)
         ! It is illegal to DEALLOCATE an array that is not allocated. You can check the
         ! allocation status of an array before deallocating with the ALLOCATED function.
         ! severe (726): DEALLOCATE: Character array not allocated
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6772)
         ! There is not enough memory space to hold the array.
         ! Dynamic memory allocation is limited by several factors, including swap file
         ! size and memory requirements of other applications that are running. If you
         ! encounter an unexpectedly low limit, you might need to reset your virtual
         ! memory size through the Windows* Control Panel or redefine the swap file size.
         ! Allocated arrays that are no longer needed should be deallocated.
         ! severe (727): Cannot ALLOCATE allocatable array -- out of memory
         call Exception$throwHeapException(HeapExceptions$IOS_F6772)
      case(FOR$IOS_F6773)
         ! There is not enough memory space to hold the automatic data object.
         ! Dynamic memory allocation is limited by several factors, including swap file
         ! size and memory requirements of other applications that are running. If you
         ! encounter an unexpectedly low limit, you might need to reset your virtual
         ! memory size through the Windows* Control Panel or redefine the swap file size.
         ! Allocated arrays that are no longer needed should be deallocated.
         ! An automatic data object is an object that is declared in a procedure
         ! subprogram or interface, is not a dummy argument, and depends on a nonconstant
         ! expression. For example:
         !   SUBROUTINE  EXAMPLE (N)
         !     DIMENSION A (N, 5), B(10*N)
         ! The arrays A and B in the example are automatic data objects.
         ! severe (728): Cannot allocate automatic object -- out of memory
         call Exception$throwHeapException(HeapExceptions$IOS_F6773)
      case(FOR$IOS_F6774)
         ! It is illegal to DEALLOCATE an array that is not allocated. You can check the
         ! allocation status of an array before deallocating with the ALLOCATED function.
         ! severe (729): DEALLOCATE failure: ALLOCATABLE array is not ALLOCATED
         throw(EAccessViolation(AMessage, AError))
      case(FOR$IOS_F6775)
         ! The optional argument DIM specifies the dimension along which minimum values
         ! are returned, and must be greater than or equal to 1 and less than or equal to
         ! the number of dimensions in the array. That is, 1 <= DIM <= n, where n is the
         ! number of dimensions in array.
         ! severe (730): Out of range: DIM argument to MINVAL has value 'dim'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6776)
         ! The optional argument DIM specifies the dimension along which minimum values
         ! are returned, and must be greater than or equal to 1 and less than or equal to
         ! the number of dimensions (rank) in the array. That is, 1 <= DIM <= n, where
         ! n is the number of dimensions in array.
         ! severe (731): Out of range: DIM argument to MINVAL has value 'dim' with ARRAY
         ! of rank 'rank'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6777)
         ! MOD(A,P) is computed as A - INT(A,P) * P. P cannot be zero.
         ! severe (732): P argument to MOD is double precision zero
         throw(EZeroDivide(AMessage, AError))
      case(FOR$IOS_F6778)
         ! MOD(A,P) is computed as A - INT(A,P) * P. P cannot be zero.
         ! severe (733): P argument to MOD is integer zero
         throw(EDivByZero(AMessage, AError))
      case(FOR$IOS_F6779)
         ! MOD(A,P) is computed as A - INT(A,P) * P. P cannot be zero.
         ! severe (734): P argument to MOD is real zero
         throw(EZeroDivide(AMessage, AError))
      case(FOR$IOS_F6780)
         ! MODULO(A,P) for real numbers is computed as A - FLOOR(A,P) * P. So, P cannot
         ! be zero.
         ! severe (735): P argument to MODULO is real zero
         throw(EZeroDivide(AMessage, AError))
      case(FOR$IOS_F6781)
         ! In the function, MODULO(A,P), P cannot be zero.
         ! severe (736): P argument to MODULO is zero
         throw(EDivByZero(AMessage, AError))
      case(FOR$IOS_F6782)
         ! The sign of the S argument to NEAREST(X,S) determines the direction of the
         ! search for the nearest number to X, and cannot be zero.
         ! severe (737): Argument S to NEAREST is zero
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6783)
         ! severe (738): Heap storage exhausted
         call Exception$throwHeapException(HeapExceptions$IOS_F6783)
      case(FOR$IOS_F6784)
         ! The integer array PUT must be greater than or equal to the number of integers
         ! the processor uses to set the seed value. This number can be determined by
         ! calling RANDOM_SEED with the SIZE argument. For example:
         !   INTEGER, ALLOCATABLE SEED
         !   CALL RANDOM_SEED( )           ! initialize processor
         !   CALL RANDOM_SEED(SIZE = K)    ! get size of seed
         !   ALLOCATE SEED(K)              ! allocate array
         !   CALL RANDOM_SEED(PUT = SEED)  ! set the seed
         ! Note
         ! RANDOM_SEED can be called with at most one argument at a time.
         ! severe (739): PUT argument to RANDOM_SEED is too small
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6785)
         ! The integer array GET must be greater than or equal to the number of integers
         ! the processor uses to set the seed value. This number can be determined by
         ! calling RANDOM_SEED with the SIZE argument. For example:
         !   INTEGER, ALLOCATABLE SEED
         !   CALL RANDOM_SEED( )           ! initialize processor
         !   CALL RANDOM_SEED(SIZE = K)    ! get size of seed
         !   ALLOCATE SEED(K)              ! allocate array
         !   CALL RANDOM_SEED(GET = SEED)  ! get the seed
         ! Note
         ! RANDOM_SEED can be called with at most one argument at a time.
         ! severe (740): GET argument to RANDOM_SEED is too small
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6786)
        ! severe (741): Recursive I/O reference
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6787)
         ! severe (742): Argument to SHAPE intrinsic is not PRESENT
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6788)
         ! The optional argument DIM specifies the dimension whose upper bound is to be
         ! returned, and must be greater than or equal to 1 and less than or equal to the
         ! number of dimensions in the array. That is, 1 <= DIM <= n, where n is the
         ! number of dimensions in array.
         ! severe (743): Out of range: DIM argument to UBOUND had value 'dim'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6789)
         ! The optional argument DIM specifies the dimension whose upper bound is to be
         ! returned, and must be greater than or equal to 1 and less than or equal to the
         ! number of dimensions (rank) in the array. That is, 1 <= DIM <= n, where n is
         ! the number of dimensions in array.
         ! severe (744): DIM argument ('dim') to UBOUND greater than ARRAY rank 'rank'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6790)
         ! The optional argument DIM specifies the dimension whose upper bound is to be
         ! returned.
         ! An assumed-size array is a dummy argument in a subroutine or function, and the
         ! upper bound of its last dimension is determined by the size of actual array
         ! passed to it. Assumed-size arrays have no determined shape, and you cannot use
         ! UBOUND to determine the extent of the last dimension. You can use UBOUND to
         ! determine the upper bound of one of the fixed dimensions, in which case you
         ! must pass the dimension number along with the array name.
         ! severe (745): Out of range: UBOUND of assumed-size array with DIM==rank
         ! ('rank')
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6791)
         ! The optional argument DIM specifies the dimension whose upper bound is to be
         ! returned, and must be greater than or equal to 1 and less than or equal to the
         ! number of dimensions (rank) in the array. That is, 1 <= DIM <= n, where n is
         ! the number of dimensions in array.
         ! severe (746): Out of range: DIM argument ('dim') to UBOUND greater than ARRAY
         ! rank
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6792)
         ! severe (747): Shape mismatch: Dimension 'shape' extents are 'ext1' and 'ext2'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6793)
         ! An illegal value was used with the POSITION specifier.
         ! POSITION accepts the following values:
         !     'ASIS' (the default)
         !     'REWIND' - on Fortran I/O systems, this is the same as 'ASIS'
         !     'APPEND'
         ! severe (748): Illegal POSITION value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6794)
         ! An illegal value was used with the ACTION specifier.
         ! ACTION accepts the following values:
         !     'READ'
         !     'WRITE'
         !     'READWRITE' - the default
         ! severe (749): Illegal ACTION value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6795)
         ! The DELIM specifier is only allowed for files connected for formatted data
         ! transfer. It is used to delimit character constants in list-directed an
         ! namelist output.
         ! severe (750): DELIM= specifier not allowed for an UNFORMATTED file
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6796)
         ! An illegal value was used with the DELIM specifier.
         ! DELIM accepts the following values:
         !     'APOSTROPHE'
         !     'QUOTE'
         !     'NONE' - the default
         ! severe (751): Illegal DELIM value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6797)
         ! The PAD specifier is only allowed for formatted input records. It indicates
         ! whether the formatted input record is padded with blanks when an input list
         ! and format specification requires more data than the record contains.
         ! severe (752): PAD= specifier not allowed for an UNFORMATTED file
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6798)
         ! An illegal value was used with the PAD specifier.
         ! PAD accepts the following values:
         !     'NO'
         !     'YES' - the default
         ! severe (753): Illegal PAD= value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6799)
         ! An illegal value was used with the CARRIAGECONTROL specifier.
         ! CARRIAGECONTROL accepts the following values:
         !     'FORTRAN' - default if the unit is connected to a terminal or console
         !     'LIST' - default for formatted files
         !     'NONE' - default for unformatted files
         ! severe (754): Illegal CARRIAGECONTROL=value
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6800)
         ! The SIZE specifier can only appear in a formatted, sequential READ statement
         ! that has the specifier ADVANCE='NO' (indicating non-advancing input).
         ! severe (755): SIZE= specifier only allowed with ADVANCE='NO'
         throw(EInvalidOpException(AMessage, AError))
      case(FOR$IOS_F6801)
         ! severe (756): Illegal character in binary input
         throw(EConvertError(AMessage, AError))
      case(FOR$IOS_F6802)
         ! severe (757): Illegal character in octal input
         throw(EConvertError(AMessage, AError))
      case(FOR$IOS_F6803)
         ! severe (758): End of record encountered
         throw(EInOutError(AMessage, AError))
      case(FOR$IOS_F6804)
         ! severe (759): Illegal subscript in namelist input record
         throw(ENameListError(AMessage, AError))
      case default
         throw(EExternalException("Unknown error: " // AMessage, AError))
   end select
end function