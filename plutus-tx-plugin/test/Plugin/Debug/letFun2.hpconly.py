(program
  fromList []
  (let
    fromList []
    (nonrec)
    (datatypebind
      fromList []
      (datatype
        fromList []
        (tyvardecl fromList [] Bool (fromList [] type))

        Bool_match
        (vardecl fromList [] True fromList [] Bool)
        (vardecl fromList [] False fromList [] Bool)
      )
    )
    (termbind
      fromList []
      (strict)
      (vardecl
        fromList []
        equalsInteger
        (fun
          fromList []
          (con fromList [] integer)
          (fun fromList [] (con fromList [] integer) (con fromList [] bool))
        )
      )
      (builtin fromList [] equalsInteger)
    )
    (termbind
      fromList []
      (strict)
      (vardecl
        fromList []
        ifThenElse
        (all
          fromList []
          a
          (fromList [] type)
          (fun
            fromList []
            (con fromList [] bool)
            (fun
              fromList []
              fromList [] a
              (fun fromList [] fromList [] a fromList [] a)
            )
          )
        )
      )
      (builtin fromList [] ifThenElse)
    )
    (termbind
      fromList []
      (strict)
      (vardecl
        fromList []
        equalsInteger
        (fun
          fromList []
          (con fromList [] integer)
          (fun fromList [] (con fromList [] integer) fromList [] Bool)
        )
      )
      (lam
        fromList []
        x
        (con fromList [] integer)
        (lam
          fromList []
          y
          (con fromList [] integer)
          [
            fromList []
            [
              fromList []
              [
                fromList []
                { fromList [] fromList [] ifThenElse fromList [] Bool }
                [
                  fromList []
                  [ fromList [] fromList [] equalsInteger fromList [] x ]
                  fromList [] y
                ]
              ]
              fromList [] True
            ]
            fromList [] False
          ]
        )
      )
    )
    (lam
      fromList [SrcSpan {srcSpanFile = "test/Plugin/Debug/Spec.hs", srcSpanSLine = 39, srcSpanSCol = 9, srcSpanELine = 39, srcSpanECol = 59}]
      ds
      (con fromList [] integer)
      [
        fromList [SrcSpan {srcSpanFile = "test/Plugin/Debug/Spec.hs", srcSpanSLine = 39, srcSpanSCol = 31, srcSpanELine = 39, srcSpanECol = 58}]
        [
          fromList []
          fromList [] equalsInteger
          fromList [SrcSpan {srcSpanFile = "test/Plugin/Debug/Spec.hs", srcSpanSLine = 39, srcSpanSCol = 54, srcSpanELine = 39, srcSpanECol = 56}]
          ds
        ]
        (con
          fromList [SrcSpan {srcSpanFile = "test/Plugin/Debug/Spec.hs", srcSpanSLine = 39, srcSpanSCol = 58, srcSpanELine = 39, srcSpanECol = 58}]
          integer
          3
        )
      ]
    )
  )
)