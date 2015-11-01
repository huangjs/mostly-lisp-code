#include <clang-c/Index.h>
#include "fixclang.h"

const char * fixclang_getCString(const CXString * string)
{
        return clang_getCString(*string);
}

void fixclang_disposeString(const CXString * string)
{
        return clang_disposeString(*string);
}

void fixclang_getFileName(CXString * name, CXFile file)
{
        *name = clang_getFileName(file);
}

void fixclang_getNullLocation(CXSourceLocation * location)
{
        *location = clang_getNullLocation();
}

unsigned fixclang_equalLocations(const CXSourceLocation * loc1, 
                                 const CXSourceLocation * loc2)
{
        return clang_equalLocations(*loc1, *loc2);
}

void fixclang_getLocation(CXSourceLocation * location,
                          CXTranslationUnit tu,
                          CXFile file,
                          unsigned line,
                          unsigned column)
{
        *location = clang_getLocation(tu, file, line, column);
}

void fixclang_getLocationForOffset(CXSourceLocation * location,
                                   CXTranslationUnit TU,
                                   CXFile file,
                                   unsigned offset)
{
        *location = clang_getLocationForOffset(TU, file, offset);
}

void fixclang_getNullRange(CXSourceRange * range)
{
        *range = clang_getNullRange();
}

void fixclang_getRange(CXSourceRange * range,
                       const CXSourceLocation * loc1,
                       const CXSourceLocation * loc2)
{
        *range = clang_getRange(*loc1, *loc2);
}

unsigned fixclang_equalRanges(const CXSourceRange * range1,
                          const CXSourceRange * range2)
{
        return clang_equalRanges(*range1, *range2);
}

#if 0
void fixclang_getExpansionLocation(const CXSourceLocation * location,
                                   CXFile * file,
                                   unsigned * line,
                                   unsigned * column,
                                   unsigned * offset)
{
        return clang_getExpansionLocation(*location,
                                          file, line, column, offset);
}
#endif

void fixclang_getPresumedLocation(const CXSourceLocation * location,
                                  CXString * filename,
                                  unsigned * line,
                                  unsigned * column)
{
        return clang_getPresumedLocation(*location,
                                         filename, line, column);
}

void fixclang_getInstantiationLocation(const CXSourceLocation * location,
                                       CXFile * file,
                                       unsigned * line,
                                       unsigned * column,
                                       unsigned * offset)
{
        return clang_getInstantiationLocation(*location,
                                              file, line, column, offset);
}

void fixclang_getSpellingLocation(const CXSourceLocation * location,
                                  CXFile * file,
                                  unsigned * line,
                                  unsigned * column,
                                  unsigned * offset)
{
        return clang_getSpellingLocation(*location,
                                         file, line, column, offset);
}

void fixclang_getRangeStart(CXSourceLocation * start,
                            const CXSourceRange * range)
{
        *start = clang_getRangeStart(*range);
}

void fixclang_getRangeEnd(CXSourceLocation * end,
                          const CXSourceRange * range)
{
        *end = clang_getRangeEnd(*range);
}

void fixclang_formatDiagnostic(CXString * string,
                               CXDiagnostic diagnostic,
                               unsigned options)
{
        *string = clang_formatDiagnostic(diagnostic, options);
}

void fixclang_getDiagnosticLocation(CXSourceLocation * loc,
                                    CXDiagnostic diagnostic)
{
        *loc = clang_getDiagnosticLocation(diagnostic);
}

void fixclang_getDiagnosticSpelling(CXString * string,
                                    CXDiagnostic diagnostic)
{
        *string = clang_getDiagnosticSpelling(diagnostic);
}

void fixclang_getDiagnosticOption(CXString * string,
                                  CXDiagnostic diagnostic,
                                  CXString * disable)
{
        *string = clang_getDiagnosticOption(diagnostic, disable);
}

void fixclang_getDiagnosticCategoryName(CXString * string,
                                        unsigned category)
{
        *string = clang_getDiagnosticCategoryName(category);
}

void fixclang_getDiagnosticRange(CXSourceRange * OUT_range,
                                 CXDiagnostic diagnostic,
                                 unsigned range)
{
        *OUT_range = clang_getDiagnosticRange(diagnostic, range);
}

void fixclang_getDiagnosticFixIt(CXString * string,
                                 CXDiagnostic diagnostic,
                                 unsigned fixit,
                                 CXSourceRange * replacement)
{
        *string = clang_getDiagnosticFixIt(diagnostic,
                                           fixit,
                                           replacement);
}

void fixclang_getTranslationUnitSpelling(CXString * string,
                                         CXTranslationUnit TU)
{
        *string = clang_getTranslationUnitSpelling(TU);
}

void fixclang_getCXTUResourceUsage(CXTUResourceUsage * rusage,
                                   CXTranslationUnit TU)
{
        *rusage = clang_getCXTUResourceUsage(TU);
}

void fixclang_disposeCXTUResourceUsage(const CXTUResourceUsage * rusage)
{
        return clang_disposeCXTUResourceUsage(*rusage);
}

void fixclang_getNullCursor(CXCursor * cursor)
{
        *cursor = clang_getNullCursor();
}

void fixclang_getTranslationUnitCursor(CXCursor * cursor,
                                       CXTranslationUnit TU)
{
        *cursor = clang_getTranslationUnitCursor(TU);
}

unsigned fixclang_equalCursors(const CXCursor * cursor1,
                               const CXCursor * cursor2)
{
        return clang_equalCursors(*cursor1, *cursor2);
}

unsigned fixclang_hashCursor(const CXCursor * cursor)
{
        return clang_hashCursor(*cursor);
}

enum CXCursorKind fixclang_getCursorKind(const CXCursor * cursor)
{
        return clang_getCursorKind(*cursor);
}

enum CXLinkageKind fixclang_getCursorLinkage(const CXCursor * cursor)
{
        return clang_getCursorLinkage(*cursor);
}

enum CXAvailabilityKind fixclang_getCursorAvailability(const CXCursor * cursor)
{
        return clang_getCursorAvailability(*cursor);
}

enum CXLanguageKind fixclang_getCursorLanguage(const CXCursor * cursor)
{
        return clang_getCursorLanguage(*cursor);
}

unsigned fixclang_CXCursorSet_contains(CXCursorSet cset,
                                       const CXCursor * cursor)
{
        return clang_CXCursorSet_contains(cset, *cursor);
}

unsigned fixclang_CXCursorSet_insert(CXCursorSet cset,
                                     const CXCursor * cursor)
{
        return clang_CXCursorSet_insert(cset, *cursor);
}

void fixclang_getCursorSemanticParent(CXCursor * parent,
                                      const CXCursor * cursor)
{
        *parent = clang_getCursorSemanticParent(*cursor);
}

void fixclang_getCursorLexicalParent(CXCursor * parent,
                                      const CXCursor * cursor)
{
        *parent = clang_getCursorLexicalParent(*cursor);
}

void fixclang_getOverriddenCursors(const CXCursor * cursor,
                                   CXCursor **overridden,
                                   unsigned * num_overridden)
{
        return clang_getOverriddenCursors(*cursor, 
                                          overridden, 
                                          num_overridden);
}

CXFile fixclang_getIncludedFile(const CXCursor * cursor)
{
        return clang_getIncludedFile(*cursor);
}

void fixclang_getCursor(CXCursor * cursor,
                        CXTranslationUnit TU,
                        const CXSourceLocation * location)
{
        *cursor = clang_getCursor(TU, *location);
}

void fixclang_getCursorLocation(CXSourceLocation * location,
                                const CXCursor * cursor)
{
        *location = clang_getCursorLocation(*cursor);
}

void fixclang_getCursorExtent(CXSourceRange * range,
                              const CXCursor * cursor)
{
        *range = clang_getCursorExtent(*cursor);
}

void fixclang_getCursorType(CXType * type,
                            const CXCursor * cursor)
{
        *type = clang_getCursorType(*cursor);
}

unsigned fixclang_equalTypes(const CXType * A, const CXType * B)
{
        return clang_equalTypes(*A, *B);
}

void fixclang_getCanonicalType(CXType * canonical,
                               const CXType * type)
{
        *canonical = clang_getCanonicalType(*type);
}

unsigned fixclang_isConstQualifiedType(const CXType * type)
{
        return clang_isConstQualifiedType(*type);
}

unsigned fixclang_isVolatileQualifiedType(const CXType * type)
{
        return clang_isVolatileQualifiedType(*type);
}

unsigned fixclang_isRestrictQualifiedType(const CXType * type)
{
        return clang_isRestrictQualifiedType(*type);
}

void fixclang_getPointeeType(CXType * pointee,
                             const CXType * type)
{
        *pointee = clang_getPointeeType(*type);
}

void fixclang_getTypeDeclaration(CXCursor * cursor,
                                 const CXType * type)
{
        *cursor = clang_getTypeDeclaration(*type);
}

void fixclang_getDeclObjCTypeEncoding(CXString * string,
                                      const CXCursor * cursor)
{
        *string = clang_getDeclObjCTypeEncoding(*cursor);
}

void fixclang_getTypeKindSpelling(CXString * string,
                                  enum CXTypeKind kind)
{
        *string = clang_getTypeKindSpelling(kind);
}

void fixclang_getResultType(CXType * result,
                            const CXType * function_type)
{
        *result = clang_getResultType(*function_type);
}

void fixclang_getCursorResultType (CXType * type,
                                   const CXCursor * cursor)
{
        *type = clang_getCursorResultType(*cursor);
}

unsigned fixclang_isPODType(const CXType * type)
{
        return clang_isPODType(*type);
}

unsigned fixclang_isVirtualBase(const CXCursor * cursor)
{
        return clang_isVirtualBase(*cursor);
}

enum CX_CXXAccessSpecifier
fixclang_getCXXAccessSpecifier(const CXCursor * cursor)
{
        return clang_getCXXAccessSpecifier(*cursor);
}

unsigned fixclang_getNumOverloadedDecls(const CXCursor * cursor)
{
        return clang_getNumOverloadedDecls(*cursor);
}

void fixclang_getOverloadedDecl(CXCursor * overloaded,
                                const CXCursor * cursor,
                                unsigned index)
{
        *overloaded = clang_getOverloadedDecl(*cursor, index);
}

void fixclang_getIBOutletCollectionType(CXType * type,
                                        const CXCursor * cursor)
{
        *type = clang_getIBOutletCollectionType(*cursor);
}

struct visitor_data {
        FCXCursorVisitor visitor;
        CXClientData data;
};

static enum CXChildVisitResult
fixclang_visitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
        struct visitor_data data = *(struct visitor_data*)client_data;
        return data.visitor(&cursor, &parent, data.data);
}

unsigned fixclang_visitChildren(const CXCursor * parent,
                                FCXCursorVisitor visitor,
                                CXClientData client_data)
{
        struct visitor_data data = {visitor, client_data};
        return clang_visitChildren(*parent, fixclang_visitor, &data);
}

void fixclang_getCursorUSR(CXString * USR, const CXCursor * cursor)
{
        *USR = clang_getCursorUSR(*cursor);
}

void fixclang_constructUSR_ObjCClass(CXString * USR,
                                     const char * class_name)
{
        *USR = clang_constructUSR_ObjCClass(class_name);
}

void fixclang_constructUSR_ObjCCategory(CXString * USR,
                                        const char * class_name,
                                        const char * category_name)
{
        *USR = clang_constructUSR_ObjCCategory(class_name, category_name);
}

void fixclang_constructUSR_ObjCProtocol(CXString * USR,
                                        const char * protocol_name)
{
        *USR = clang_constructUSR_ObjCProtocol(protocol_name);
}

void fixclang_constructUSR_ObjCIvar(CXString * USR,
                                    const char * name,
                                    const CXString * classUSR)
{
        *USR = clang_constructUSR_ObjCIvar(name, *classUSR);
}

void fixclang_constructUSR_ObjCMethod(CXString * USR,
                                      const char * name,
                                      unsigned isInstanceMethod,
                                      const CXString * classUSR)
{
        *USR = clang_constructUSR_ObjCMethod(name, isInstanceMethod,
                                             *classUSR);
}

void fixclang_constructUSR_ObjCProperty(CXString *USR,
                                        const char * property,
                                        const CXString * classUSR)
{
        *USR = clang_constructUSR_ObjCProperty(property, *classUSR);
}

void fixclang_getCursorSpelling(CXString * string,
                                const CXCursor * cursor)
{
        *string = clang_getCursorSpelling(*cursor);
}

void fixclang_getCursorDisplayName(CXString * string,
                                   const CXCursor * cursor)
{
        *string = clang_getCursorDisplayName(*cursor);
}

void fixclang_getCursorReferenced(CXCursor * referenced,
                                  const CXCursor * cursor)
{
        *referenced = clang_getCursorReferenced(*cursor);
}

void fixclang_getCursorDefinition(CXCursor * definition,
                                  const CXCursor * cursor)
{
        *definition = clang_getCursorDefinition(*cursor);
}

unsigned fixclang_isCursorDefinition(const CXCursor * cursor)
{
        return clang_isCursorDefinition(*cursor);
}

void fixclang_getCanonicalCursor(CXCursor * canonical,
                                 const CXCursor * cursor)
{
        *canonical = clang_getCanonicalCursor(*cursor);
}

unsigned fixclang_CXXMethod_isStatic(const CXCursor * cursor)
{
        return clang_CXXMethod_isStatic(*cursor);
}

unsigned fixclang_CXXMethod_isVirtual (const CXCursor * cursor)
{
        return clang_CXXMethod_isVirtual(*cursor);
}

enum CXCursorKind fixclang_getTemplateCursorKind(const CXCursor * cursor)
{
        return clang_getTemplateCursorKind(*cursor);
}

void fixclang_getSpecializedCursorTemplate(CXCursor * specializee,
                                           const CXCursor * cursor)
{
        *specializee = clang_getSpecializedCursorTemplate(*cursor);
}

void fixclang_getCursorReferenceNameRange(CXSourceRange * range,
                                          const CXCursor * cursor,
                                          unsigned NameFlags,
                                          unsigned PieceIndex)
{
        *range = clang_getCursorReferenceNameRange(*cursor,
                                                   NameFlags, 
                                                   PieceIndex);
}

CXTokenKind fixclang_getTokenKind(const CXToken * token)
{
        return clang_getTokenKind(*token);
}

void fixclang_getTokenSpelling(CXString * string,
                               CXTranslationUnit TU,
                               const CXToken * token)
{
        *string = clang_getTokenSpelling(TU, *token);
}

void fixclang_getTokenLocation(CXSourceLocation * location,
                               CXTranslationUnit TU,
                               const CXToken * token)
{
        *location = clang_getTokenLocation(TU, *token);
}

void fixclang_getTokenExtent(CXSourceRange * range,
                             CXTranslationUnit TU,
                             const CXToken * token)
{
        *range = clang_getTokenExtent(TU, *token);
}

void fixclang_tokenize(CXTranslationUnit TU,
                       const CXSourceRange * range,
                       CXToken **tokens, unsigned * NumTokens)
{
        return clang_tokenize(TU, *range,
                              tokens, NumTokens);
}

void fixclang_getCursorKindSpelling(CXString * spelling,
                                    enum CXCursorKind kind)
{
        *spelling = clang_getCursorKindSpelling(kind);
}

void fixclang_getDefinitionSpellingAndExtent(const CXCursor * definition,
                                             const char ** startBuf,
                                             const char ** endBuf,
                                             unsigned * startLine,
                                             unsigned * startColumn,
                                             unsigned * endLine,
                                             unsigned * endColumn)
{
        return clang_getDefinitionSpellingAndExtent(*definition,
                                                    startBuf,
                                                    endBuf,
                                                    startLine,
                                                    startColumn,
                                                    endLine,
                                                   endColumn);
}

void fixclang_getCompletionChunkText(CXString * text,
                                     CXCompletionString completion_string,
                                     unsigned chunk_number)
{
        *text = clang_getCompletionChunkText(completion_string, chunk_number);
}

CXCompletionString fixclang_getCursorCompletionString(const CXCursor * cursor)
{
        return clang_getCursorCompletionString(*cursor);
}

void fixclang_codeCompleteGetContainerUSR(CXString * USR,
                                          CXCodeCompleteResults *Results)
{
        *USR = clang_codeCompleteGetContainerUSR(Results);
}

void fixclang_codeCompleteGetObjCSelector(CXString * selector,
                                          CXCodeCompleteResults *Results)
{
        *selector = clang_codeCompleteGetObjCSelector(Results);
}

void fixclang_getClangVersion(CXString * version)
{
        *version = clang_getClangVersion();
}
