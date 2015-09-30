module Test.TypeCheck where

import Test.TestUtils

typeCheckTests =
    [ success "Test/TypeCheck/PkgImpl.pkg"
    , failure "Test/TypeCheck/PkgImplErr.pkg"
    , success "Test/TypeCheck/IfaceSubtype.pkg"
    , failure "Test/TypeCheck/IfaceSubtypeErr.pkg"
    , success "Test/TypeCheck/Sharing.pkg"
    , failure "Test/TypeCheck/SharingErr.pkg"
    , success "Test/TypeCheck/PkgImport.pkg"
    , failure "Test/TypeCheck/PkgImportErr.pkg"
    ]
