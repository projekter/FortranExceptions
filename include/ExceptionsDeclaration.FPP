/**
  * Provide shorthands for defining a new exception type
  */
#define declareException(newType, parentType) \
   type, extends(parentType) :: newType; \
   end type; \
   interface newType; \
      module procedure newType##$create; \
   end interface; \
   private :: newType##$create
#define implementException(newType) \
   function newType##$create(AMessage, ACode) result(Res); \
      character(*), intent(in), optional :: AMessage; \
      integer, intent(in), optional :: ACode; \
      class(Exception), pointer :: Res; \
\
      allocate(newType :: Res); \
      if(present(AMessage)) then; \
         Res%Message = AMessage; \
      else; \
         Res%Message = ""; \
      end if; \
      if(present(ACode)) then; \
         Res%Code = ACode; \
      end if; \
   end function
#define DECLAREEXCEPTION(newType, parentType) declareException(newType, parentType)
#define declareexception(newType, parentType) declareException(newType, parentType)
#define DeclareException(newType, parentType) declareException(newType, parentType)
#define Declareexception(newType, parentType) declareException(newType, parentType)
#define IMPLEMENTEXCEPTION(newType) implementException(newType)
#define implementexception(newType) implementException(newType)
#define ImplementException(newType) implementException(newType)
#define Implementexception(newType) implementException(newType)