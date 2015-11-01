#ifndef FIXCLANG_C_H
#define FIXCLANG_C_H
#include <clang-c/Index.h>

const char * fixclang_getCString(const CXString * string);
void fixclang_disposeString(const CXString * string);

void fixclang_getFileName(CXString * name, CXFile file);

void fixclang_getNullLocation(CXSourceLocation * location);
unsigned fixclang_equalLocations(const CXSourceLocation * loc1,
                                 const CXSourceLocation * loc2);
void fixclang_getLocation(CXSourceLocation * location,
                          CXTranslationUnit tu,
                          CXFile file,
                          unsigned line,
                          unsigned column);
void fixclang_getLocationForOffset(CXSourceLocation * location,
                                   CXTranslationUnit tu,
                                   CXFile file,
                                   unsigned offset);

void fixclang_getNullRange(CXSourceRange * range);
void fixclang_getRange(CXSourceRange * range,
                       const CXSourceLocation * begin, 
                       const CXSourceLocation * end);
unsigned fixclang_equalRanges(const CXSourceRange * range1,
                              const CXSourceRange * range2);
#if 0
/* This symbol is local on my build of HEAD... */
void fixclang_getExpansionLocation(const CXSourceLocation * location,
                                   CXFile * file,
                                   unsigned * line,
                                   unsigned * column,
                                   unsigned * offset);
#endif
void fixclang_getPresumedLocation(const CXSourceLocation * location,
                                  CXString * filename,
                                  unsigned * line,
                                  unsigned * column);
void fixclang_getInstantiationLocation(const CXSourceLocation * location,
                                       CXFile * file,
                                       unsigned * line,
                                       unsigned * column,
                                       unsigned * offset);
void fixclang_getSpellingLocation(const CXSourceLocation * location,
                                  CXFile * file,
                                  unsigned * line,
                                  unsigned * column,
                                  unsigned * offset);
void fixclang_getRangeStart(CXSourceLocation * start,
                            const CXSourceRange * range);
void fixclang_getRangeEnd(CXSourceLocation * end,
                          const CXSourceRange * range);

void fixclang_formatDiagnostic(CXString * string,
                               CXDiagnostic diagnostic,
                               unsigned options);
void fixclang_getDiagnosticLocation(CXSourceLocation * loc, 
                                    CXDiagnostic diagnostic);
void fixclang_getDiagnosticSpelling(CXString * string,
                                    CXDiagnostic diagnostic);
void fixclang_getDiagnosticOption(CXString * string,
                                  CXDiagnostic diagnostic,
                                  CXString * disable);
void fixclang_getDiagnosticCategoryName(CXString * string,
                                        unsigned category);
void fixclang_getDiagnosticRange(CXSourceRange * OUT_range,
                                 CXDiagnostic diagnostic,
                                 unsigned range);
void fixclang_getDiagnosticFixIt(CXString * string,
                                 CXDiagnostic diagnostic,
                                 unsigned fixit,
                                 CXSourceRange * replacement);

void fixclang_getTranslationUnitSpelling(CXString * string, CXTranslationUnit tu);

void fixclang_getCXTUResourceUsage(CXTUResourceUsage * rusage, CXTranslationUnit TU);
void fixclang_disposeCXTUResourceUsage(const CXTUResourceUsage * rusage);

void fixclang_getNullCursor(CXCursor * cursor);
void fixclang_getTranslationUnitCursor(CXCursor * cursor, CXTranslationUnit TU);
unsigned fixclang_equalCursors(const CXCursor * cursor1, const CXCursor * cursor2);
unsigned fixclang_hashCursor(const CXCursor * cursor);
enum CXCursorKind fixclang_getCursorKind(const CXCursor * cursor);
enum CXLinkageKind fixclang_getCursorLinkage(const CXCursor * cursor);
enum CXAvailabilityKind fixclang_getCursorAvailability(const CXCursor * cursor);
enum CXLanguageKind fixclang_getCursorLanguage(const CXCursor * cursor);

unsigned fixclang_CXCursorSet_contains(CXCursorSet cset, const CXCursor * cursor);
unsigned fixclang_CXCursorSet_insert(CXCursorSet cset, const CXCursor * cursor);

void fixclang_getCursorSemanticParent(CXCursor * parent, const CXCursor * cursor);
void fixclang_getCursorLexicalParent(CXCursor * parent, const CXCursor * cursor);
void fixclang_getOverriddenCursors(const CXCursor * cursor, 
                                   CXCursor **overridden, 
                                   unsigned * num_overridden);

CXFile fixclang_getIncludedFile(const CXCursor * cursor);
void fixclang_getCursor(CXCursor * cursor, 
                        CXTranslationUnit TU,
                        const CXSourceLocation * location);
void fixclang_getCursorLocation(CXSourceLocation * location,
                                const CXCursor * cursor);
void fixclang_getCursorExtent(CXSourceRange * range,
                              const CXCursor * cursor);

void fixclang_getCursorType(CXType * type, const CXCursor * cursor);
unsigned fixclang_equalTypes(const CXType * A, const CXType * B);
void fixclang_getCanonicalType(CXType * canonical, const CXType * type);
unsigned fixclang_isConstQualifiedType(const CXType * type);
unsigned fixclang_isVolatileQualifiedType(const CXType * type);
unsigned fixclang_isRestrictQualifiedType(const CXType * type);
void fixclang_getPointeeType(CXType * pointee, const CXType * type);
void fixclang_getTypeDeclaration(CXCursor * cursor, const CXType * type);
void fixclang_getDeclObjCTypeEncoding(CXString * string,  const CXCursor * cursor);
void fixclang_getTypeKindSpelling(CXString * string, enum CXTypeKind kind);
void fixclang_getResultType(CXType * result, const CXType * function_type);
void fixclang_getCursorResultType(CXType * result, const CXCursor * cursor);
unsigned fixclang_isPODType(const CXType * type);
unsigned fixclang_isVirtualBase(const CXCursor * cursor);
enum CX_CXXAccessSpecifier fixclang_getCXXAccessSpecifier(const CXCursor * cursor);
unsigned fixclang_getNumOverloadedDecls(const CXCursor * cursor);
void fixclang_getOverloadedDecl(CXCursor * overloaded,
                                const CXCursor * cursor,
                                unsigned index);
void fixclang_getIBOutletCollectionType(CXType * type, const CXCursor * cursor);

typedef enum CXChildVisitResult (*FCXCursorVisitor)(const CXCursor * cursor,
                                                    const CXCursor * parent,
                                                    CXClientData client_data);
unsigned fixclang_visitChildren(const CXCursor * parent,
                                FCXCursorVisitor visitor,
                                CXClientData client_data);

void fixclang_getCursorUSR(CXString * USR, const CXCursor * cursor);
void fixclang_constructUSR_ObjCClass(CXString * USR, const char * class_name);
void fixclang_constructUSR_ObjCCategory(CXString * USR, 
                                        const char * class_name,
                                        const char * category_name);
void fixclang_constructUSR_ObjCProtocol(CXString * USR, const char * protocol_name);
void fixclang_constructUSR_ObjCIvar(CXString * USR, const char * name,
                                    const CXString * classUSR);
void fixclang_constructUSR_ObjCMethod(CXString * USR,
                                      const char * name,
                                      unsigned isInstanceMethod,
                                      const CXString * classUSR);
void fixclang_constructUSR_ObjCProperty(CXString * USR,
                                        const char * property,
                                        const CXString * classUSR);

void fixclang_getCursorSpelling(CXString * string, const CXCursor * cursor);
void fixclang_getCursorDisplayName(CXString * string, const CXCursor * cursor);

void fixclang_getCursorReferenced(CXCursor * referenced, const CXCursor * cursor);
void fixclang_getCursorDefinition(CXCursor * definition, const CXCursor * cursor);
unsigned fixclang_isCursorDefinition(const CXCursor * cursor);
void fixclang_getCanonicalCursor(CXCursor * canonical, const CXCursor * cursor);

unsigned fixclang_CXXMethod_isStatic(const CXCursor * cursor);
unsigned fixclang_CXXMethod_isVirtual(const CXCursor * cursor);
enum CXCursorKind fixclang_getTemplateCursorKind(const CXCursor * cursor);
void fixclang_getSpecializedCursorTemplate(CXCursor * specializee,
                                           const CXCursor * cursor);
void fixclang_getCursorReferenceNameRange(CXSourceRange * range,
                                          const CXCursor * cursor,
                                          unsigned NameFlags,
                                          unsigned PieceIndex);

CXTokenKind fixclang_getTokenKind(const CXToken * token);
void fixclang_getTokenSpelling(CXString * string,
                               CXTranslationUnit TU,
                               const CXToken * token);
void fixclang_getTokenLocation(CXSourceLocation * location,
                               CXTranslationUnit TU,
                               const CXToken * token);
void fixclang_getTokenExtent(CXSourceRange * extent,
                             CXTranslationUnit TU,
                             const CXToken * token);
void fixclang_tokenize(CXTranslationUnit TU, const CXSourceRange * range,
                       CXToken **Tokens, unsigned * NumTokens);

void fixclang_getCursorKindSpelling(CXString * spelling, enum CXCursorKind kind);
void fixclang_getDefinitionSpellingAndExtent(const CXCursor * definition,
                                             const char ** startBuf,
                                             const char ** endBuf,
                                             unsigned * startLine,
                                             unsigned * startColumn,
                                             unsigned * endLine,
                                             unsigned * endColumn);

void fixclang_getCompletionChunkText(CXString * text,
                                     CXCompletionString completion_string,
                                     unsigned chunk_number);
CXCompletionString fixclang_getCursorCompletionString(const CXCursor * cursor);

void fixclang_codeCompleteGetContainerUSR(CXString * USR, CXCodeCompleteResults *Results);
void fixclang_codeCompleteGetObjCSelector(CXString * selector, CXCodeCompleteResults *Results);
void fixclang_getClangVersion(CXString * version);
#endif
