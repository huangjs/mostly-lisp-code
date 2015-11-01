(load-shared-object "~/cl-clang/libfixclang.dylib")
(load-shared-object "~/lib/libclang.dylib")

(in-package "CLANG-FFI")
(define-alien-type index (* t))
(define-alien-type translation-unit (* t))
(define-alien-type unsaved-file
    (struct nil
            (filename c-string)
            (contents c-string)
            (length   unsigned-long)))

(define-alien-type availability-kind
    (enum availability-kind
          available
          deprecated
          not-available))
(define-alien-type string
    (struct nil
            (data  (* t))
            (flags unsigned)))

(define-alien-routine ("fixclang_getCString" get-string) c-string
  (string string :copy))

(define-alien-routine ("fixclang_disposeString" dispose-string) void
  (string string :copy))

(define-alien-routine ("clang_createIndex" create-index) index
  (exclude-declarations-from-PCH int)
  (display-diagnostics int))

(define-alien-routine ("clang_disposeIndex" dispose-index) void
  (index index))

(define-alien-type file (* t))

(define-alien-routine ("fixclang_getFileName" get-file-name) nil
  (string string :out)
  (sfile file))
(define-alien-routine ("clang_getFileTime" get-file-time) sb-unix:time-t (sfile file))

(define-alien-routine ("clang_isFileMultipleIncludeGuarded" is-file-multiple-include-guarded)
    unsigned
  (translation-unit translation-unit)
  (file file))

(define-alien-routine ("clang_getFile" get-file) file
  (translation-unit translation-unit)
  (file-name c-string))

(define-alien-type source-location
    (struct nil
            (ptr-data (array (* t) 2))
            (int-data unsigned)))

(define-alien-type source-range
    (struct nil
            (ptr-data (array (* t) 2))
            (begin-int-data unsigned)
            (end-int-data unsigned)))

(define-alien-routine ("fixclang_getNullLocation" get-null-location) nil
  (location source-location :out))

(define-alien-routine ("fixclang_equalLocations" equal-locations) unsigned
  (loc1 source-location :copy)
  (loc2 source-location :copy))

(define-alien-routine ("fixclang_getLocation" get-location) nil
  (location source-location :out)
  (translation-unit translation-unit)
  (file file)
  (line unsigned)
  (column unsigned))

(define-alien-routine ("fixclang_getLocationForOffset" get-location-for-offset) nil
  (location source-location :out)
  (translation-unit translation-unit)
  (file file)
  (offset unsigned))

(define-alien-routine ("fixclang_getNullRange" get-null-range) nil
  (range source-range :out))

(define-alien-routine ("fixclang_getRange" get-range) nil
  (range source-range :out)
  (begin source-location :copy)
  (end   source-location :copy))

(define-alien-routine ("fixclang_equalRanges" equal-ranges) unsigned
  (range1 source-range :copy)
  (range2 source-range :copy))

;; unfortunately, not exposed from my libclang.dylib...
#+nil
(define-alien-routine ("fixclang_getExpansionLocation" get-expansion-location) nil
  (location source-location :copy)
  (file file :out)
  (line unsigned :out)
  (column unsigned :out)
  (offset unsigned :out))

(define-alien-routine ("fixclang_getPresumedLocation" get-presumed-location) nil
  (location source-location :copy)
  (filename string :out)
  (line     unsigned :out)
  (column   unsigned :out))

(define-alien-routine ("fixclang_getInstantiationLocation" get-instantiation-location) nil
  (location source-location :copy)
  (file file :out)
  (line unsigned :out)
  (column unsigned :out)
  (offset unsigned :out))

(define-alien-routine ("fixclang_getSpellingLocation" get-spelling-location) nil
  (location source-location :copy)
  (file file :copy)
  (line unsigned :out)
  (column unsigned :out)
  (offset unsigned :out))

(define-alien-routine ("fixclang_getRangeStart" get-range-start) nil
  (location source-location :out)
  (range    source-range :copy))

(define-alien-routine ("fixclang_getRangeEnd" get-range-end) nil
  (location source-location :out)
  (range    source-range :copy))

(define-alien-type diagnostic-severity
    (enum diagnostic-severity
          ignored
          note
          warning
          error
          fatal))

(define-alien-type diagnostic (* t))

(define-alien-routine ("clang_getNumDiagnostics" get-num-diagnostics) unsigned
  (translation-unit translation-unit))

(define-alien-routine ("clang_getDiagnostic" get-diagnostic) diagnostic
  (translation-unit translation-unit)
  (index unsigned))

(define-alien-routine ("clang_disposeDiagnostic" dispose-diagnostic) void
  (diagnostic diagnostic))

(macrolet ((def (&rest names)
               `(progn
                  ,@(loop for name in names
                          for value = 1 then (* 2 value)
                          collect
                          `(defconstant ,name ,value)))))
  (def diagnostic-display-source-location
      diagnostic-display-column
    diagnostic-display-source-ranges
    diagnostic-display-option
    diagnostic-display-category-id
    diagnostic-display-category-name))

(define-alien-routine ("fixclang_formatDiagnostic" format-diagnostic) nil
  (string string :out)
  (diagnostic diagnostic)
  (options unsigned))

(define-alien-routine ("clang_defaultDiagnosticDisplayOptions" default-diagnostic-display-options)
    unsigned)

(define-alien-routine ("clang_getDiagnosticSeverity" get-diagnostic-severity) diagnostic-severity
  (diagnostic diagnostic))

(define-alien-routine ("fixclang_getDiagnosticLocation" get-diagnostic-location) nil
  (location source-location :out)
  (diagnostic diagnostic))

(define-alien-routine ("fixclang_getDiagnosticSpelling" get-diagnostic-spelling) nil
  (spelling string :out)
  (diagnostic diagnostic))

(define-alien-routine ("fixclang_getDiagnosticOption" get-diagnostic-option) nil
  (option string :out)
  (diagnostic diagnostic)
  (disable string :out))

(define-alien-routine ("clang_getDiagnosticCategory" get-diagnostic-category) unsigned
  (diagnostic diagnostic))

(define-alien-routine ("fixclang_getDiagnosticCategoryName" get-diagnostic-category-name) nil
  (name string :out)
  (category unsigned))

(define-alien-routine ("clang_getDiagnosticNumRanges" get-diagnostic-num-ranges) unsigned
  (diagnostic diagnostic))

(define-alien-routine ("fixclang_getDiagnosticRange" get-diagnostic-range) nil
  (source-range source-range :out)
  (diagnostic diagnostic)
  (range unsigned))

(define-alien-routine ("clang_getDiagnosticNumFixIts" get-diagnostic-num-fix-its) unsigned
  (diagnostic diagnostic))

(define-alien-routine ("fixclang_getDiagnosticFixIt" get-diagnostic-fix-it) nil
  (string string :out)
  (diagnostic diagnostic)
  (fix-it unsigned)
  (replacement source-range :out))

(define-alien-routine ("fixclang_getTranslationUnitSpelling" get-translation-unit-spelling)
    nil
  (string string :out)
  (translation-unit translation-unit))

(define-alien-routine ("clang_createTranslationUnitFromSourceFile" create-translation-unit-from-source-file)
    translation-unit
  (index index)
  (source-file-name c-string)
  (num-clang-command-line-args int)
  (clang-command-line-args (* c-string))
  (num-unsaved-files unsigned)
  (unsaved-files (* unsaved-file)))

(define-alien-routine ("clang_createTranslationUnit" create-translation-unit)
    translation-unit
  (index index)
  (ast-file-name c-string))

(macrolet ((def (&rest names)
               `(progn
                  ,@(loop for name in names
                          for value = 1 then (* 2 value)
                          collect
                          `(defconstant ,name ,value)))))
  (defconstant translation-unit-none 0)
  (def translation-unit-detailed-preprocessing-record
       translation-unit-incomplete
    translation-unit-precompiled-preamble
    translation-unit-cache-completion-results
    translation-unit-CXX-precompiled-preamble
    translation-unit-CXX-chained-PCH
    translation-unit-nested-macro-expansions))

(define-alien-routine ("clang_defaultEditingTranslationUnitOptions" default-editing-translation-unit-options)
    unsigned)

(define-alien-routine ("clang_parseTranslationUnit" parse-translation-unit)
    translation-unit
  (index index)
  (source-file-name c-string)
  (command-line-args (* c-string))
  (num-command-line-args int)
  (unsaved-files (* unsaved-file))
  (num-unsaved-files unsigned)
  (options unsigned))

(defconstant save-translation-unit-none 0)

(define-alien-routine ("clang_defaultSaveOptions" default-save-options) unsigned
  (translation-unit translation-unit))

(define-alien-type save-error
    (enum save-error
          none
          unknown
          translation-errors
          invalid-translation-unit))

(define-alien-routine ("clang_saveTranslationUnit" save-translation-unit)
    save-error
  (translation-unit translation-unit)
  (file-name c-string)
  (options unsigned))

(define-alien-routine ("clang_disposeTranslationUnit" dispose-translation-unit)
    void
  (translation-unit translation-unit))

(defconstant reparse-none 0)

(define-alien-routine ("clang_defaultReparseOptions" default-reparse-options)
    unsigned (translation-unit translation-unit))

(define-alien-routine ("clang_reparseTranslationUnit" reparse-translation-unit)
    int
  (translation-unit translation-unit)
  (num-unsaved-files unsigned)
  (unsaved-files (* unsaved-file))
  (options unsigned))

(define-alien-type resource-usage-kind
    (enum nil
          (ast 1)
          identifiers
          selectors
          global-completion-results
          source-manager-content-cache
          ast-side-tables
          source-manager-membuffer-malloc
          source-manager-membuffer-mmap
          external-ast-source-membuffer-malloc
          external-ast-source-membuffer-mmap
          preprocessor
          preprocessing-record
          source-manager-data-structure
          preprocessor-header-search))

(define-alien-routine ("clang_getTUResourceUsageName" get-translation-unit-resource-name)
    c-string
  (kind resource-usage-kind))

(define-alien-type translation-unit-resource-usage-entry
    (struct nil
            (kind resource-usage-kind)
            (amount unsigned-long)))

(define-alien-type translation-unit-resource-usage
    (struct nil
            (data (* t))
            (num-entries unsigned)
            (entries (* translation-unit-resource-usage-entry))))

(define-alien-routine ("fixclang_getCXTUResourceUsage" get-translation-unit-resource-usage)
    nil
  (usage translation-unit-resource-usage :out)
  (translation-unit translation-unit))

(define-alien-routine ("fixclang_disposeCXTUResourceUsage" dispose-translation-unit-resource-usage)
    void
  (usage translation-unit-resource-usage :copy))

(define-alien-type cursor-kind
    (enum nil
          (unexposed-decl 1)
          struct-decl
          union-decl
          class-decl
          enum-decl

          field-decl
          enum-constant-decl

          function-decl
          var-decl
          parm-decl
          
          obj-c-interface-decl
          obj-c-category-decl
          obj-c-protocol-decl
          obj-c-property-decl
          obj-c-ivar-decl
          obj-c-instance-method-decl
          obj-c-class-method-decl
          obj-c-implementation-decl
          obj-c-category-impl-decl

          typedef-decl

          cxx-method
          namespace

          linkage-spec

          constructor
          destructor
          conversion-function
          template-type-parameter
          non-type-template-parameter
          template-template-parameter
          function-template
          class-template
          class-template-partial-specialization
          
          namespace-alias
          using-directive
          using-declaration
          type-alias-decl

          obj-c-synthesize-decl
          obj-c-dynamic-decl

          (obj-c-super-class-ref 40)
          obj-c-protocol-ref
          obj-c-class-ref
          type-ref
          cxx-base-specifier
          template-ref
          namespace-ref
          member-ref
          label-ref
          overloaded-decl-ref

          (invalid-file 70)
          no-decl-found
          not-implemented
          invalid-code

          (unexposed-expr 100)
          decl-ref-expr
          member-ref-expr
          call-expr
          obj-c-message-expr
          block-expr

          (unexposed-stmt 200)
          label-stmt

          (translation-unit 300)

          (unexposed-attr 400)
          ib-action-attr
          ib-outlet-attr
          ib-outlet-collection-attr
          CXX-final-attr
          CXX-override-attr

          (preprocessing-directive 500)
          macro-definition
          macro-expansion
          inclusion-directive))

(define-alien-type cursor
    (struct nil
            (kind cursor-kind)
            (data (array (* t) 3))))

(define-alien-routine ("fixclang_getNullCursor" get-null-cursor) nil
  (cursor cursor :out))

(define-alien-routine ("fixclang_getTranslationUnitCursor" get-translation-unit-cursor) nil
  (cursor cursor :out)
  (translation-unit translation-unit))

(define-alien-routine ("fixclang_equalCursors" equal-cursors) unsigned
  (cursor-1 cursor :copy)
  (cursor-2 cursor :copy))

(define-alien-routine ("fixclang_hashCursor" hash-cursor) unsigned
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCursorKind" get-cursor-kind) cursor-kind
  (cursor cursor :copy))

(define-alien-routine ("clang_isDeclaration" is-declaration) unsigned
  (kind cursor-kind))
(define-alien-routine ("clang_isReference" is-reference) unsigned
  (kind cursor-kind))
(define-alien-routine ("clang_isExpression" is-expression) unsigned
  (kind cursor-kind))
(define-alien-routine ("clang_isStatement" is-statement) unsigned
  (kind cursor-kind))
(define-alien-routine ("clang_isAttribute" is-attribute) unsigned
  (kind cursor-kind))
(define-alien-routine ("clang_isInvalid" is-invalid) unsigned
  (kind cursor-kind))
(define-alien-routine ("clang_isTranslationUnit" is-translation-unit) unsigned
  (kind cursor-kind))
(define-alien-routine ("clang_isPreprocessing" is-preprocessing) unsigned
  (kind cursor-kind))
(define-alien-routine ("clang_isUnexposed" is-unexposed) unsigned
  (kind cursor-kind))

(define-alien-type linkage-kind
    (enum nil
          invalid
          no-linkage
          internal
          unique-external
          external))

(define-alien-routine ("fixclang_getCursorLinkage" get-cursor-linkage) linkage-kind
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCursorAvailability" get-cursor-availability) availability-kind
  (cursor cursor :copy))

(define-alien-type language-kind
    (enum nil
          invalid
          C
          obj-c
          c++))

(define-alien-routine ("fixclang_getCursorLanguage" get-cursor-language) language-kind
  (cursor cursor :copy))

(define-alien-type cursor-set (* t))

(define-alien-routine ("clang_createCXCursorSet" create-cursor-set) cursor-set)
(define-alien-routine ("clang_disposeCXCursorSet" dispose-cursor-set) void
  (cursor-set cursor-set))
(define-alien-routine ("fixclang_CXCursorSet_contains" cursor-set-contains) unsigned
  (cursor-set cursor-set)
  (cursor cursor :copy))
(define-alien-routine ("fixclang_CXCursorSet_insert" cursor-set-insert) unsigned
  (cursor-set cursor-set)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCursorSemanticParent" get-cursor-semantic-parent) nil
  (parent cursor :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCursorLexicalParent" get-cursor-lexical-parent) nil
  (parent cursor :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getOverriddenCursors" get-overridden-cursors) nil
  (cursor cursor :copy)
  (overridden (* cursor) :out)
  (num-overridden unsigned :out))

(define-alien-routine ("clang_disposeOverriddenCursors" dispose-overridden-cursors) void
  (overridden (* cursor)))

(define-alien-routine ("fixclang_getIncludedFile" get-included-file) file
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCursor" get-cursor) nil
  (cursor cursor :out)
  (translation-unit translation-unit)
  (location source-location :copy))

(define-alien-routine ("fixclang_getCursorLocation" get-cursor-location) nil
  (location source-location :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCursorExtent" get-cursor-extent) nil
  (extent source-range :out)
  (cursor cursor :copy))

(define-alien-type type-kind
    (enum nil
          invalid
          unexposed
          void
          bool
          char-u
          u-char
          char-16
          char-32
          u-short
          u-int
          u-long
          u-long-long
          u-int-128
          char-s
          s-char
          w-char
          short
          int
          long
          long-long
          int-128
          float
          double
          long-double
          null-ptr
          overload
          dependent
          obj-c-id
          obj-c-class
          obj-c-sel

          (complex 100)
          pointer
          block-pointer
          l-value-reference
          r-value-reference
          record
          enum
          typedef
          obj-c-interface
          obj-c-object-pointer
          function-no-proto
          function-proto))

(define-alien-type type
    (struct nil
            (kind type-kind)
            (data (array (* t) 2))))

(define-alien-routine ("fixclang_getCursorType" get-cursor-type) nil
  (type type :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_equalTypes" equal-types) unsigned
  (type-a type :copy)
  (type-b type :copy))

(define-alien-routine ("fixclang_getCanonicalType" get-canonical-type) nil
  (canonical type :out)
  (type type :copy))

(define-alien-routine ("fixclang_isConstQualifiedType" is-const-qualified-type) unsigned
  (type type :copy))
(define-alien-routine ("fixclang_isVolatileQualifiedType" is-volatile-qualified-type) unsigned
  (type type :copy))
(define-alien-routine ("fixclang_isRestrictQualifiedType" is-restrict-qualified-type) unsigned
  (type type :copy))

(define-alien-routine ("fixclang_getPointeeType" get-pointee-type) nil
  (pointee type :out)
  (type    type :copy))

(define-alien-routine ("fixclang_getTypeDeclaration" get-type-declaration) nil
  (declaration cursor :out)
  (type        type :copy))

(define-alien-routine ("fixclang_getDeclObjCTypeEncoding" get-decl-obj-c-type-encoding) nil
  (encoding string :out)
  (cursor   cursor :copy))

(define-alien-routine ("fixclang_getTypeKindSpelling" get-type-kind-spelling) nil
  (spelling string :out)
  (kind type-kind))

(define-alien-routine ("fixclang_getResultType" get-result-type) nil
  (result-type type :out)
  (function-type type :copy))

(define-alien-routine ("fixclang_getCursorResultType" get-cursor-result-type) nil
  (result-type type :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_isPODType" is-pod-type) unsigned
  (type type :copy))

(define-alien-routine ("fixclang_isVirtualBase" is-virtual-base) unsigned
  (cursor cursor :copy))

(define-alien-type cxx-access-specifier
    (enum nil
          invalid
          public
          protected
          private))

(define-alien-routine ("fixclang_getCXXAccessSpecifier" get-cxx-access-specifier)
    cxx-access-specifier
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getNumOverloadedDecls" get-num-overloaded-decls) unsigned
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getOverloadedDecl" get-overloaded-decl) nil
  (overloaded cursor :out)
  (cursor cursor :copy)
  (index unsigned))

(define-alien-routine ("fixclang_getIBOutletCollectionType" get-ib-outlet-collection-type) nil
  (type type :out)
  (cursor cursor :copy))

(define-alien-type child-visit-result
    (enum nil
          break
          continue
          recurse))

(defconstant break 'break)
(defconstant continue 'continue)
(defconstant recurse 'recurse)

(sb-alien::define-alien-callback ast-visitor-callback child-visit-result
    ((cursor (* cursor))
     (parent (* cursor))
     (data   (* t)))
  (let* ((function (sb-kernel:%make-lisp-obj (sb-sys:sap-int
                                              (alien-sap data))))
         (value (funcall function cursor parent)))
    (ecase value
      (break 0)
      (continue 1)
      (recurse 2))))

(define-alien-routine ("fixclang_visitChildren" visit-children) unsigned
  (parent  cursor :copy)
  (visitor (* t))
  (data    (* t)))

(define-alien-routine ("fixclang_getCursorUSR" get-cursor-USR) nil
  (string string :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_constructUSR_ObjCClass" construct-USR-obj-c-class) nil
  (string string :out)
  (class c-string))

(define-alien-routine ("fixclang_constructUSR_ObjCCategory" construct-USR-obj-c-category) nil
  (string string :out)
  (class c-string)
  (category c-string))

(define-alien-routine ("fixclang_constructUSR_ObjCProtocol" construct-USR-obj-c-protocol) nil
  (string string :out)
  (protocol c-string))

(define-alien-routine ("fixclang_constructUSR_ObjCMethod" construct-USR-obj-c-method) nil
  (string string :out)
  (name c-string)
  (is-instance-method unsigned)
  (class-usr string :copy))

(define-alien-routine ("fixclang_constructUSR_ObjCProperty" construct-USR-obj-c-property) nil
  (string string :out)
  (name c-string)
  (class-usr string :copy))

(define-alien-routine ("fixclang_getCursorSpelling" get-cursor-spelling) nil
  (spelling string :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCursorDisplayName" get-cursor-display-name) nil
  (display-name string :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCursorReferenced" get-cursor-referenced) nil
  (referenced cursor :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCursorDefinition" get-cursor-definition) nil
  (definition cursor :out)
  (cursor cursor :copy))

(define-alien-routine ("fixclang_isCursorDefinition" is-cursor-definition) unsigned
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getCanonicalCursor" get-canonical-cursor) nil
  (canonical cursor :out)
  (cursor    cursor :copy))

(define-alien-routine ("fixclang_CXXMethod_isStatic" cxx-method-is-static) unsigned
  (cursor cursor :copy))
(define-alien-routine ("fixclang_CXXMethod_isVirtual" cxx-method-is-virtual) unsigned
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getTemplateCursorKind" get-template-cursor-kind) cursor-kind
  (cursor cursor :copy))

(define-alien-routine ("fixclang_getSpecializedCursorTemplate" get-specialized-cursor-template)
    nil
  (specialised cursor :out)
  (cursor      cursor :copy))

(define-alien-routine ("fixclang_getCursorReferenceNameRange" get-cursor-reference-name-range)
    nil
  (source-range source-range :out)
  (cursor     cursor :copy)
  (name-flags unsigned)
  (piece-index unsigned))

(macrolet ((def (&rest names)
               `(progn
                  ,@(loop for name in names
                          for value = 1 then (* 2 value)
                          collect
                          `(defconstant ,name ,value)))))
  (def name-range-want-qualifier
      name-range-want-template-args
    name-range-want-single-piece))

(define-alien-type token-kind
    (enum nil
          punctuation
          keyword
          identifier
          literal
          comment))

(define-alien-type token
    (struct nil
            (int-data (array unsigned 4))
            (ptr-data (* t))))

(define-alien-routine ("fixclang_getTokenKind" get-token-kind) token-kind
  (token token :copy))

(define-alien-routine ("fixclang_getTokenSpelling" get-token-spelling) nil
  (spelling string :out)
  (translation-unit translation-unit)
  (token  token :copy))

(define-alien-routine ("fixclang_getTokenLocation" get-token-location) nil
  (location source-location :out)
  (translation-unit translation-unit)
  (token  token :copy))

(define-alien-routine ("fixclang_getTokenExtent" get-token-extent) nil
  (extent source-range :out)
  (translation-unit translation-unit)
  (token  token :copy))

(define-alien-routine ("fixclang_tokenize" tokenize) nil
  (translation-unit translation-unit)
  (range  source-range :copy)
  (tokens (* token)   :out)
  (num-token unsigned :out))

(define-alien-routine ("clang_annotateTokens" annotate-tokens) void
  (translation-unit translation-unit)
  (tokens (* token))
  (num-token unsigned)
  (cursors (* cursor)))

(define-alien-routine ("clang_disposeTokens" dispose-tokens) void
  (translation-unit translation-unit)
  (tokens (* token))
  (num-tokens unsigned))

(define-alien-routine ("fixclang_getCursorKindSpelling" get-cursor-kind-spelling) nil
  (string string :out)
  (kind cursor-kind))

(define-alien-routine ("fixclang_getDefinitionSpellingAndExtent" get-definition-spelling-and-extent) nil
  (cursor cursor :copy)
  (start-buf (* char) :out)
  (end-buf   (* char) :out)
  (start-line unsigned :out)
  (start-column unsigned :out)
  (end-line unsigned :out)
  (end-column unsigned :out))

(define-alien-routine ("clang_enableStackTraces" enable-stack-traces) void)
(define-alien-routine ("clang_executeOnThread" execute-on-thread) void
  (function (* t))
  (user-data (* t))
  (stack-size unsigned))

(define-alien-type completion-string (* t))

(define-alien-type completion-result
    (struct nil
            (kind cursor-kind)
            (string completion-string)))

(define-alien-type completion-chunk-kind
    (enum nil
          optional
          typed-text
          text
          placeholder
          informative
          current-parameter
          left-paren
          right-paren
          left-bracket
          right-bracket
          left-brace
          right-brace
          left-angle
          right-angle
          comma
          result-type
          colon
          semicolon
          equal
          horizontal-space
          vertical-space))

(define-alien-routine ("clang_getCompletionChunkKind" get-completion-chunk-kind)
    completion-chunk-kind
  (completion-string completion-string)
  (chunk-number unsigned))

(define-alien-routine ("fixclang_getCompletionChunkText" get-completion-chunk-text)
    nil
  (text string :out)
  (completion-string completion-string)
  (chunk-number unsigned))

(define-alien-routine ("clang_getCompletionChunkCompletionString" get-completion-chunk-completion-string)
    completion-string
  (completion-string completion-string)
  (chunk-number unsigned))

(define-alien-routine ("clang_getNumCompletionChunks" get-num-completion-chunks) unsigned
  (completion-string completion-string))

(define-alien-routine ("clang_getCompletionPriority" get-completion-priority) unsigned
  (completion-string completion-string))

(define-alien-routine ("clang_getCompletionAvailability" get-completion-availability)
    availability-kind
  (completion-string completion-string))

(define-alien-routine ("fixclang_getCursorCompletionString" get-cursor-completion-string)
    completion-string
  (cursor cursor :copy))

(define-alien-type code-complete-results
    (struct nil
            (results (* completion-result))
            (num-results unsigned)))

(macrolet ((def (&rest names)
               `(progn
                  ,@(loop for name in names
                          for value = 1 then (* 2 value)
                          collect
                          `(defconstant ,name ,value)))))
  (def code-complete-include-macros
      code-complete-include-code-patterns))

(macrolet ((def (&rest names)
               `(progn
                  ,@(loop for name in names
                          for value = 1 then (* 2 value)
                          collect
                          `(defconstant ,name ,value)))))
  (defconstant completion-context-unexposed 0)
  (def completion-context-any-type
      completion-context-any-value
    completion-context-obj-c-object-value
    completion-context-obj-c-selector-value
    completion-context-cxx-class-type-value
    completion-context-dot-member-access
    completion-context-arrow-member-access
    completion-context-obj-c-property-access
    completion-context-enum-tag
    completion-context-union-tag
    completion-context-struct-tag
    completion-context-class-tag
    completion-context-namespace
    completion-context-nested-name-specifier
    completion-context-obj-c-interface
    completion-context-obj-c-protocol
    completion-context-obj-c-category
    completion-context-obj-c-instance-message
    completion-context-obj-c-class-message
    completion-context-obj-c-selector-name
    completion-context-macro-name
    completion-context-natural-language
    completion-context-unknown))

(define-alien-routine ("clang_defaultCodeCompleteOptions" default-code-complete-options) unsigned)

(define-alien-routine ("clang_codeCompleteAt" code-complete-at) (* code-complete-results)
  (translation-unit translation-unit)
  (complete-file-name c-string)
  (complete-line unsigned)
  (complete-column unsigned)
  (unsaged-files (* unsaved-file))
  (num-unsaved-files unsigned)
  (options unsigned))

(define-alien-routine ("clang_sortCodeCompletionResults" sort-code-completion-reults) void
  (results completion-result :copy)
  (num-results unsigned))

(define-alien-routine ("clang_disposeCodeCompleteResults" dispose-code-complete-results) void
  (results code-complete-results :copy))

(define-alien-routine ("clang_codeCompleteGetNumDiagnostics" code-complete-get-num-diagnostics)
    unsigned
  (results code-complete-results :copy))

(define-alien-routine ("clang_codeCompleteGetDiagnostic" code-complete-get-diagnostic)
    diagnostic
  (results code-complete-results :copy)
  (index unsigned))

(define-alien-routine ("clang_codeCompleteGetContexts" code-complete-get-contexts)
    unsigned-long-long
  (results code-complete-results :copy))

(define-alien-routine ("clang_codeCompleteGetContainerKind" code-complete-get-container-kind)
    cursor-kind
  (results code-complete-results :copy)
  (is-incomplete unsigned :out))

(define-alien-routine ("fixclang_codeCompleteGetContainerUSR" code-complete-get-container-USR)
    nil
  (usr string :out)
  (results code-complete-results :copy))

(define-alien-routine ("fixclang_codeCompleteGetObjCSelector" code-complete-get-obj-c-selector)
    nil
  (selector string :out)
  (results  code-complete-results :copy))

(define-alien-routine ("fixclang_getClangVersion" get-clang-version) nil
  (version string :out))

(define-alien-routine ("clang_toggleCrashRecovery" toggle-crash-recovery) void
  (is-enabled unsigned))

(sb-alien::define-alien-callback inclusion-visitor-callback void
    ((included-file file)
     (inclusion-stack (* source-location))
     (include-len unsigned)
     (data   (* t)))
  (let ((function (sb-kernel:%make-lisp-obj (sb-sys:sap-int
                                             (alien-sap data)))))
    (funcall function included-file inclusion-stack include-len)
    nil))

(define-alien-routine ("clang_getInclusions" get-inclusions) void
  (translation-unit translation-unit)
  (visitor (* t))
  (cliend-data (* t)))

(define-alien-type remapping (* t))

(define-alien-routine ("clang_getRemappings" get-remapping) remapping
  (path c-string))

(define-alien-routine ("clang_remap_getNumFiles" remap-get-num-files) unsigned
  (remapping remapping))

(define-alien-routine ("clang_remap_getFilenames" remap-get-filenames) nil
  (remapping remapping)
  (index unsigned)
  (original string :out)
  (transformed string :out))

(define-alien-routine ("clang_remap_dispose" remap-dispose) void
  (remapping remapping))

#||
CLANG-FFI> (let ((foo (lambda (cursor parent)
                        (format t "~X ~X; ~A ~A ~A~%"
                                (hash-cursor (alien-sap parent))
                                (hash-cursor (alien-sap cursor))
                                (let ((kind (get-cursor-kind (alien-sap cursor))))
                                  (list kind
                                        (let ((type (nth-value 1 (get-cursor-type (alien-sap cursor)))))
                                          (slot type 'kind))))
                                (get-string (alien-sap (nth-value 1 (get-cursor-spelling (alien-sap cursor)))))
                                (let ((extent (nth-value 1 (get-cursor-extent (alien-sap cursor)))))
                                  (when extent
                                    (list (multiple-value-list (get-spelling-location
                                                                (alien-sap (nth-value 1 (get-range-start (alien-sap extent))))
                                                                nil))
                                          (multiple-value-list (get-spelling-location
                                                                (alien-sap (nth-value 1 (get-range-end (alien-sap extent))))
                                                                nil))))))
                        recurse)))
             (let ((*print-pretty* nil))
               (sb-sys:with-pinned-objects (foo)
                 (visit-children (alien-sap *cursor*)
                                 (alien-sap ast-visitor-callback)
                                 (sb-sys:int-sap (sb-kernel:get-lisp-obj-address foo))))))
||#
